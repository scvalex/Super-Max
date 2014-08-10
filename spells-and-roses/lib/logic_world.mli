open Core.Std

include module type of Logic_world_intf

module Make(Logic_state : Logic_state) : sig
  (** The [Logic_world.t] is the container for the logic of the game.
      It is a pure finite state machine that takes events as input and
      mutates the state in a predictable way. *)
  type t

  val create :
       step : int
    -> state : Logic_state.t
    -> history_rewrite_cutoff : int
    -> t

  val add_event : t -> Logic_state.Event.t -> t Or_error.t

  val on_step : t -> t

  val state : t -> Logic_state.t
end
