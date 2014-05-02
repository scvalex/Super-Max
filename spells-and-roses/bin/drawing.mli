open Core.Std

(** A [Drawing.t] is the only thing that can be shown on a screen.

    Coordinate system: @(0.0, 0.0)@ is the top-left corner of the
    screen.
*)
type t

include Sexpable.S with type t := t

module Global : sig
  val stats : unit -> string
end

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

val text :
     font : string
  -> size_pt : int
  -> ?position : ([`X of [`Left | `Centre | `Right]]
                  * [`Y of [`Top | `Centre | `Bottom]])
  -> string
  -> t

val centered_normalized_scene :
     width : int
  -> height : int
  -> t
  -> t

val render : t -> renderer : Sdlrender.t -> unit

module Example : sig
  (** [rectangles] are a bunch of rectangles that cover a 1.0 side
      square. *)
  val rectangles :
       width : int
    -> height : int
    -> t
end
