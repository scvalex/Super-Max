open Core.Std
open Rarray_intf

type ('elt, 'bigarray_elt) t

val length : (_, _) t -> int

val iteri :
  ('elt, _) t
  -> f : (int -> 'elt -> unit)
  -> unit

val data : ('elt, 'bigarray_elt) t -> ('elt, 'bigarray_elt, Bigarray.c_layout) Bigarray.Array1.t

val size_bytes : (_, _) t -> int

module Make(X : S_in) : S

module Int : S
  with type elt = int32
  with type bigarray_elt = Bigarray.int32_elt
  with type t = (int32, Bigarray.int32_elt) t

module Float : S
  with type elt = float
  with type bigarray_elt = Bigarray.float32_elt
  with type t = (float, Bigarray.float32_elt) t
