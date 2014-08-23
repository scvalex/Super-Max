open Core.Std
open Async.Std
open Game_intf

let client_id_counter = ref 0;;

let make_client_id () =
  let id = !client_id_counter in
  incr client_id_counter;
  Client_id.of_string (sprintf "client-%d" id)
;;

module Make(Query : Binable_and_sexpable)
    (Update : Binable_and_sexpable)
    (Snapshot : Binable_and_sexpable) =
struct
  module Event = struct
    type t =
      [ `Query of (Query.t * Client_id.t * Snapshot.t Query_response.t Ivar.t)
      | `Update of Update.t
      | `Disconnected of Client_id.t
      ]
  end

  module Update_ext = struct
    type t =
      | Snapshot of Snapshot.t
      | Update of Update.t
    with bin_io
  end

  type t = {
    clients       : Update_ext.t Pipe.Writer.t Client_id.Table.t;
    events        : Event.t Pipe.Reader.t;
    events_writer : Event.t Pipe.Writer.t;
  } with fields

  let updates_rpc =
    Rpc.Pipe_rpc.create
      ~name:"Super_max_lib.Broadcast_hub.Updates"
      ~version:1
      ~bin_query:Query.bin_t
      ~bin_response:Update_ext.bin_t
      ~bin_error:Error.bin_t
      ()
  ;;

  let implementations =
    let on_unknown_rpc ~rpc_tag ~version =
      Mlog.m "Unknown RPC: %s v%d" rpc_tag version
    in
    let implementations =
      [ Rpc.Pipe_rpc.implement updates_rpc (fun (t, address) query ~aborted ->
         let client_id = make_client_id () in
         Mlog.m "Incoming updates RPC from %s (%s)"
           (Client_id.to_string client_id)
           (Socket.Address.to_string address);
         let response = Ivar.create () in
         Pipe.write t.events_writer (`Query (query, client_id, response))
         >>= fun () ->
         Ivar.read response
         >>| function
         | `Reject reason ->
           Error (Error.of_string reason)
         | `Accept snapshot ->
           let (update_reader, update_writer) = Pipe.create () in
           upon aborted (fun () ->
             Pipe.close_read update_reader);
           upon (Pipe.closed update_writer) (fun () ->
             Hashtbl.remove t.clients client_id);
           Clock.every (sec 10.0) ~stop:aborted (fun () ->
             if Int.(Pipe.length update_writer > 10) then
               Mlog.m "Client %s pushing back"
                 (Client_id.to_string client_id));
           Pipe.write_without_pushback update_writer
             (Update_ext.Snapshot snapshot);
           Hashtbl.set t.clients ~key:client_id ~data:update_writer;
           Ok update_reader)
      ]
    in
    Rpc.Implementations.create_exn
      ~implementations
      ~on_unknown_rpc:(`Call on_unknown_rpc)
  ;;

  let create () =
    let clients = Client_id.Table.create () in
    let (events, events_writer) = Pipe.create () in
    let t = { clients; events; events_writer; } in
    let port = 7007 in
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

  let broadcast t updates =
    let updates =
      (Queue.map updates ~f:(fun update ->
         Update_ext.Update update))
    in
    Hashtbl.iter t.clients ~f:(fun ~key:_ ~data:update_writer ->
      if not (Pipe.is_closed update_writer) then
        Pipe.write_without_pushback' update_writer (Queue.copy updates))
  ;;

  let connect_to _t ~hostname =
    Mlog.m "Connecting to %s" hostname;
    failwith "not implemented"
  ;;
end
