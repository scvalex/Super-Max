open Core.Std

type t with sexp

(** [create] makes a new camera at origin looking down the negative Z
    axis. *)
val create : unit -> t

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
