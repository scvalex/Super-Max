open Core.Std

(** A [Drawing.t] is the only thing that can be shown on a screen.

    Coordinate system: @(0.0, 0.0)@ is the top-left corner of the
    screen.
*)
type t

include Sexpable.S with type t := t

val empty : t

val translate :
     x : float
  -> y : float
  -> t
  -> t

val scale :
     x : float
  -> y : float
  -> t
  -> t

val rectangle :
     width : float
  -> height : float
  -> t

val colour :
     r : float
  -> g : float
  -> b : float
  -> ?a : float
  -> t
  -> t

val many : t list -> t

module Example : sig
  (** [rectangles] are a bunch of rectangles that cover a 1.0 side
      square. *)
  val rectangles : t
end
