open Core.Std
open Async.Std
open Game_intf

exception Already_connected_to of (string * [`requested of string]) with sexp
exception Pipe_closed_before_snapshot of string with sexp

let peer_id_counter = ref 0;;

let make_peer_id () =
  let id = !peer_id_counter in
  incr peer_id_counter;
  Peer_id.of_string (sprintf "peer-%d" id)
;;

let port = 7007;;

module Make(Query : Binable_and_sexpable)
    (Update : Binable_and_sexpable)
    (Snapshot : Binable_and_sexpable) =
struct
  module Event = struct
    type t =
      [ `Query of (Query.t * Peer_id.t * Snapshot.t Query_response.t Ivar.t)
      | `Update of Update.t
      | `Snapshot of Snapshot.t
      | `Disconnected of Peer_id.t
      | `Unjoined
      ]
  end

  module Query_ext = struct
    type t =
      | Parent_snapshot_and_updates of Query.t
      | Child_updates
    with bin_io, sexp
  end
  open Query_ext

  module Update_ext = struct
    type t =
      | Snapshot of Snapshot.t
      | Update of Update.t
    with bin_io, sexp
  end
  open Update_ext

  exception Didn't_get_snapshot of (string * Update_ext.t) with sexp

  type t = {
    mutable parent : string option;
    children       : Update_ext.t Pipe.Writer.t Peer_id.Table.t;
    events         : Event.t Pipe.Reader.t;
    events_writer  : Event.t Pipe.Writer.t;
  } with fields

  (** Join Protocol from Child to Parent:

      C                                         P
      |                                         |
      |   Parent_snapshot_and_updates Query.t   |
      | --------------------------------------> |
      |              Child_updates              |
      | <-------------------------------------- |
      |           Snapshot Snapshot.t           |
      | <-------------------------------------- |
      |              Update_ext.t*              |
      | <-------------------------------------> |
      v                                         v
  *)
  let updates_rpc =
    Rpc.Pipe_rpc.create
      ~name:"Super_max_lib.Broadcast_hub.Updates"
      ~version:1
      ~bin_query:Query_ext.bin_t
      ~bin_response:Update_ext.bin_t
      ~bin_error:Error.bin_t
      ()
  ;;

  let event t ev=
    Pipe.write_without_pushback t.events_writer ev
  ;;

  let warn_on_pushback ?(stop = Deferred.never ()) pipe_writer fmt =
    let with_message msg =
      Clock.every (sec 10.0) ~stop (fun () ->
        let length = Pipe.length pipe_writer in
        if Int.(length > 10) then
          Mlog.m "%s (%d events queued)" msg length)
    in
    ksprintf with_message fmt
  ;;

  let subscribe_to_updates ~host ~port query =
    Mlog.m "Connecting to %s" host;
    Rpc.Connection.client ~host ~port ()
    >>= function
    | Error exn ->
      Deferred.return (Or_error.of_exn exn)
    | Ok client ->
      Rpc.Pipe_rpc.dispatch updates_rpc client query
      >>| function
      | Error err | Ok (Error err) ->
        don't_wait_for (Rpc.Connection.close client);
        Error err
      | Ok (Ok (update_reader, _)) ->
        upon (Pipe.closed update_reader) (fun () ->
          don't_wait_for (Rpc.Connection.close client));
        Ok update_reader
  ;;

  let handle_updates_rpc (t, address) query_ext ~aborted =
    let peer_id = make_peer_id () in
    Mlog.m "Incoming updates RPC from %s (%s): %s"
      (Peer_id.to_string peer_id)
      (Socket.Address.to_string address)
      (Sexp.to_string_mach (Query_ext.sexp_of_t query_ext));
    match query_ext with
    | Child_updates ->
      failwith "not implemented"
    | Parent_snapshot_and_updates query ->
      let response = Ivar.create () in
      event t (`Query (query, peer_id, response));
      Ivar.read response
      >>| function
      | `Reject reason ->
        Error (Error.of_string reason)
      | `Accept snapshot ->
        (* CR scvalex: Subscribe to child updates here. *)
        let (update_reader, update_writer) = Pipe.create () in
        upon aborted (fun () ->
          Pipe.close_read update_reader);
        upon (Pipe.closed update_writer) (fun () ->
          (* CR scvalex: This isn't quite right because there might
             be more peers on this connection. *)
          event t (`Disconnected peer_id);
          Hashtbl.remove t.children peer_id);
        warn_on_pushback update_writer ~stop:aborted
          "Child %s pushing back" (Peer_id.to_string peer_id);
        Pipe.write_without_pushback update_writer
          (Update_ext.Snapshot snapshot);
        Hashtbl.set t.children ~key:peer_id ~data:update_writer;
        Ok update_reader
  ;;

  let implementations =
    let on_unknown_rpc ~rpc_tag ~version =
      Mlog.m "Unknown RPC: %s v%d" rpc_tag version
    in
    let implementations =
      [ Rpc.Pipe_rpc.implement updates_rpc handle_updates_rpc
      ]
    in
    Rpc.Implementations.create_exn
      ~implementations
      ~on_unknown_rpc:(`Call on_unknown_rpc)
  ;;

  let create () =
    let children = Peer_id.Table.create () in
    let (events, events_writer) = Pipe.create () in
    warn_on_pushback events_writer
      "Game pushing back on events";
    let parent = None in
    let t = { children; events; events_writer; parent; } in
    Rpc.Connection.serve
      ~implementations
      ~initial_connection_state:(fun address -> (t, address))
      ~where_to_listen:(Tcp.on_port port)
      ()
    >>| fun server ->
    Mlog.m "RPC server started on port %d" port;
    upon (Pipe.closed events_writer) (fun () ->
      don't_wait_for (Tcp.Server.close server));
    t
  ;;

  (* CR scvalex: Don't broadcast back to source. *)
  let broadcast t updates =
    Hashtbl.iter t.children ~f:(fun ~key:_ ~data:update_writer ->
      if not (Pipe.is_closed update_writer) then
        Pipe.write_without_pushback' update_writer (Queue.copy updates))
  ;;

  (* CR scvalex: Don't broadcast back to source. *)
  let broadcast_updates t updates =
    let updates =
      (Queue.map updates ~f:(fun update ->
         Update update))
    in
    broadcast t updates
  ;;

  let broadcast_snapshot t snapshot =
    broadcast t (Queue.singleton (Snapshot snapshot))
  ;;

  let handle_update t update_ext =
    match update_ext with
    | Update update ->
      event t (`Update update);
      broadcast_updates t (Queue.singleton update)
    | Snapshot snapshot ->
      Mlog.m "Snapshot from parent";
      event t (`Snapshot snapshot);
      broadcast_snapshot t snapshot
  ;;

  let join t ~host query =
    match t.parent with
    | Some parent_host ->
      Deferred.return
        (Or_error.of_exn
           (Already_connected_to (parent_host, `requested host)))
    | None ->
      t.parent <- Some host;
      subscribe_to_updates ~host ~port (Parent_snapshot_and_updates query)
      >>= function
      | Error err ->
        t.parent <- None;
        Deferred.return (Error err)
      | Ok update_reader ->
        upon (Pipe.closed update_reader) (fun () ->
          t.parent <- None);
        Pipe.read update_reader
        >>| function
        | `Eof ->
          Or_error.of_exn (Pipe_closed_before_snapshot host)
        | `Ok (Snapshot snapshot) ->
          don't_wait_for
            (Pipe.iter_without_pushback update_reader ~f:(handle_update t));
          event t `Unjoined;
          Ok snapshot
        | `Ok update_ext ->
          Pipe.close_read update_reader;
          Or_error.of_exn (Didn't_get_snapshot (host, update_ext))
  ;;
end
