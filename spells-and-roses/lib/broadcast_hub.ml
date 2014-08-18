open Core.Std
open Async.Std
open Game_intf

module Make(Query : Binable_and_sexpable)
    (Update : Binable_and_sexpable)
    (Snapshot : Binable_and_sexpable)
= struct
  module Broadcast = struct
    type t =
      [ `Query of (Query.t * Client_id.t * Snapshot.t Query_response.t Ivar.t)
      | `Update of Update.t
      | `Disconnected of Client_id.t
      ]
  end

  type t = {
    clients    : Update.t Pipe.Reader.t Client_id.Table.t;
    broadcasts : Broadcast.t Pipe.Reader.t;
  } with fields

  let updates_rpc =
    Rpc.Pipe_rpc.create
      ~name:"Super_max_lib.Broadcast_hub.Updates"
      ~version:1
      ~bin_query:Query.bin_t
      ~bin_response:Update.bin_t
      ~bin_error:Error.bin_t
      ()
  ;;

  let implementations =
    let on_unknown_rpc ~rpc_tag ~version =
      Mlog.m "Unknown rpc: %s v%d" rpc_tag version
    in
    let implementations =
      [ Rpc.Pipe_rpc.implement updates_rpc (fun _state _query ~aborted:_ ->
           Mlog.m "Incoming updates rpc";
           Deferred.return (Error (Error.of_string "not implemented")))
      ]
    in
    Rpc.Implementations.create_exn
      ~implementations
      ~on_unknown_rpc:(`Call on_unknown_rpc)
  ;;

  let create () =
    let clients = Client_id.Table.create () in
    let (broadcasts, _broadcasts_writer) = Pipe.create () in
    let _ = implementations in
    Deferred.return { clients; broadcasts; }
  ;;

  let broadcast _t _updates =
    ()
  ;;
end
