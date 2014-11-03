(* open Core.Std *)

type mat3

type vec3

val id : unit -> mat3

val translate :
     x : float
  -> y : float
  -> mat3

val scale :
     x : float
  -> y : float
  -> mat3

val ( * ) : mat3 -> mat3 -> mat3

val ( *| ) : mat3 -> vec3 -> vec3

val vec3_of_xy :
     x : float
  -> y : float
  -> vec3

val xy_of_vec3 :
     vec3
  -> (float * float)
