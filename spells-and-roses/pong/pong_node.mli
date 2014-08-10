open Core.Std

module State : sig
  type t

  include Binable.S with type t := t
  include Sexpable.S with type t := t
end

type t

module Xy : sig
  type t = {
    x : float;
    y : float;
  } with sexp
end

module Bounding_box : sig
  type t = {
    top_left     : Xy.t;
    bottom_right : Xy.t;
  } with sexp
end

module Direction : sig
  type t = Up | Down with sexp, compare
end

module Player : sig
  module Id : sig
    type t = A | B with sexp

    include Comparable.S with type t := t
  end
end

module Pong_event : sig
  type t =
    | Player_join of Player.Id.t
    | Player_leave of Player.Id.t
    | Move of (Player.Id.t * Direction.t)
  with sexp, compare
end

module Event : sig
  type t with sexp

  val create :
    source : Node.Id.t
    -> step : int
    -> Pong_event.t
    -> t
end

val create :
     width : float
  -> height : float
  -> history_rewrite_cutoff : int
  -> t

val create_with_state :
     State.t
  -> history_rewrite_cutoff : int
  -> t

val state : t -> State.t

val to_drawing : t -> Drawing.t

val on_step : t -> t

val on_event : t -> Event.t -> t

val ball_bounding_box : t -> Bounding_box.t

val paddles_bounding_boxes : t -> Bounding_box.t Player.Id.Map.t
