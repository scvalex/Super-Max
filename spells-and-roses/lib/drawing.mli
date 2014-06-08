open Core.Std
open Async.Std

module Context : sig
  (** A [Context.t] is used when drawing. *)
  type t

  val create :
       renderer : Sdlrender.t
    -> thread : In_thread.Helper_thread.t
    -> data_dir : string
    -> t

  val stats : t -> string
end

(** A [Drawing.t] is the only thing that can be shown on a screen.

    Coordinate system: @(0.0, 0.0)@ is the top-left corner of the
    screen.
*)
type t

include Sexpable.S with type t := t

type text_position = ([`X of [`Left | `Centre | `Right]]
                      * [`Y of [`Top | `Centre | `Bottom]]) with sexp

type image_clip = ([`X of int] * [`Y of int]
                   * [`Width of int] * [`Height of int]) with sexp

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
  -> filled : bool
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
  -> ?position : text_position
  -> string list
  -> t

val image :
     ?clip : image_clip
  -> ?angle_deg : float
  -> string
  -> t

val centered_normalized_scene :
     width : int
  -> height : int
  -> t
  -> t

val render : t -> ctx : Context.t -> unit Deferred.t

module Example : sig
  (** [rectangles] are a bunch of rectangles that cover a 1.0 side
      square. *)
  val rectangles :
       width : int
    -> height : int
    -> t
end
