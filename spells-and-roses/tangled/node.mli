open Core.Std

include module type of Node_intf

module Make(State : State)(Event : Event) : sig
  type t

  val create :
       step : int
    -> state : State.t
    -> history_length : int
    -> t

  val add_event : t -> Event.t -> t Or_error.t

  val do_step : t -> t

  val state : t -> State.t
end
