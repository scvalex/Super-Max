open Core.Std
open Rarray_intf

module Make(X : S_in) : S

module Int : S
  with type elt = int32
  with type bigarray_elt = Bigarray.int32_elt

module Float : S
  with type elt = float
  with type bigarray_elt = Bigarray.float32_elt
