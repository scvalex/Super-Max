open Core.Std

module Event_handled = struct
  type ('state, 'update) t =
    [ `Continue of ('state * 'update option)
    | `Quit of 'update option
    ]
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

  val on_event : t -> Sdlevent.t -> (t, Update.t) Event_handled.t

  val on_step : t -> (t, Update.t) Event_handled.t

  val on_update_query :
       t
    -> Update.Query.t
    -> (t * [`Accept | `Reject of string])

  val on_update : t -> Update.t -> t

  val to_drawing : t -> Drawing.t

  val steps_per_second : float
end
