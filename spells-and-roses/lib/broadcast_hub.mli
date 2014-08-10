open Core.Std
open Async.Std
open Game_intf

module Make(Query : Binable_and_sexpable)(Update : Binable_and_sexpable) : sig
  module Broadcast : sig
    type t =
      [ `Query of (Query.t * Query_response.t Ivar.t)
      | `Update of Update.t
      | `Disconnected of Client_id.t
      ]
  end

  type t

  val create : unit -> t Deferred.t

  val broadcasts : t -> Broadcast.t Pipe.Reader.t

  val updates_rpc : (Query.t, Update.t, Error.t) Rpc.Pipe_rpc.t
end
