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

  val step : Game_state.t -> Direction.t option
end
