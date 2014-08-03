open Core.Std let _ = _squelch_unused_module_warning_

module Bounding_box = Pong_node.Bounding_box
module Id = Pong_node.Player.Id
module Direction = Pong_node.Direction

module Game_state = struct
  type t = {
    paddles : Bounding_box.t Id.Map.t;
    ball    : Bounding_box.t;
  } with sexp
end

module type S = sig
  type t

  val init :
    width : float
    -> height : float
    -> playing_as : Id.t
    -> t

  val on_step : t -> Game_state.t -> (t * Direction.t option)

  val on_event : t -> Sdlevent.t -> t
end
