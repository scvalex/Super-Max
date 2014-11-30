open Core.Std
open Linear_lib.Std

type t with sexp

(** [create] makes a new camera at origin looking down the negative Z
    axis. *)
val create : unit -> t

val position : t -> (float * float * float)

val set_position : t -> (float * float * float) -> t

val translate :
  ?x : float
  -> ?y : float
  -> ?z : float
  -> t
  -> t

val reorient :
  ?pitch : float
  -> ?yaw : float
  -> ?roll : float
  -> t
  -> t

val transformation : t -> Mat.t
