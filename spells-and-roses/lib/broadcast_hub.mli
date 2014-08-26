open Core.Std
open Async.Std
open Game_intf

val port : int

module Make(Query : Binable_and_sexpable)
    (Update : Binable_and_sexpable)
    (Snapshot : Binable_and_sexpable) :
sig
  module Event : sig
    type t =
      [ `Query of (Query.t * Peer_id.t * Snapshot.t Query_response.t Ivar.t)
      | `Update of Update.t
      | `Snapshot of Snapshot.t
      | `Disconnected of Peer_id.t
      | `Unjoined
      ]
  end

  type t

  val create : unit -> t Deferred.t

  val broadcast_updates : t -> Update.t Queue.t -> unit

  (** [events] is the pipe of events received from other nodes. *)
  val events : t -> Event.t Pipe.Reader.t

  (** [join] subscribes to updates from the given host. *)
  val join :
       t
    -> host : string
    -> Query.t
    -> Snapshot.t Deferred.Or_error.t
end
