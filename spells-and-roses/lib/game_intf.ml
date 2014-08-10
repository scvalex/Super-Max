open Core.Std

(* CR scvalex: This should be named Event_handled and should be more
   extensible. *)
module Resp = struct
  type 'a t = [`Continue of 'a | `Quit]
end

module type S = sig
  module Update : sig
    module Query : sig
      type t

      include Binable.S with type t := t
      include Sexpable.S with type t := t
    end

    (** An [Update.t] is sent from [Node.t] to [Node.t] over the
        network.  It is the only mechanism to propagate events from
        one instance of the game to another. *)
    type t

    include Binable.S with type t := t
    include Sexpable.S with type t := t
  end

  type t

  val create :
       width : int
    -> height : int
    -> t

  val on_event : t -> Sdlevent.t -> t Resp.t

  val on_step : t -> t Resp.t

  val on_update_query : t -> Update.Query.t -> (t * [`Accept | `Reject])

  val on_update : t -> Update.t -> t

  val to_drawing : t -> Drawing.t

  val steps_per_second : float
end
