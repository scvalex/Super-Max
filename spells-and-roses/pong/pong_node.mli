type t

module Direction : sig
  type t = Up | Down with sexp, compare
end

(** CR scvalex: Constructor disambiguation to the rescue. *)
module Player : sig
  module Id : sig
    type t = [`A | `B] with sexp
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
  -> t

val to_drawing : t -> Drawing.t

val step : t -> t

val on_event : t -> Event.t -> t
