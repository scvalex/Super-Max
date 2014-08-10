open Core.Std

include module type of Node_intf

module Make(State : State) : sig
  (** The [Node.t] is the container for the logic of the game.  It is
      a pure finite state machine that takes events as input and
      mutates the state in a predictable way. *)
  type t

  val create :
       step : int
    -> state : State.t
    -> history_rewrite_cutoff : int
    -> t

  val add_event : t -> State.Event.t -> t Or_error.t

  val on_step : t -> t

  val state : t -> State.t
end
