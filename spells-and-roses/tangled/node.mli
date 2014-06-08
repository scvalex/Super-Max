include module type of Node_intf

module Make(State : State)(Event : Event) : sig
  type t

  val create :
       step : int
    -> state : State.t
    -> history_length : int
    -> t

  let add_event :
       t
    -> Event.t
    -> [ `Event_not_in_range of (int * int)
       | `Step_not_in_history of int
       | `Ok of t
       ]
end
