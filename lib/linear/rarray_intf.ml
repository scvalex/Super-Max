open Core.Std let _ = _squelch_unused_module_warning_

module type S_in = sig
  type elt
  type bigarray_elt

  val bigarray_elt : (elt, bigarray_elt) Bigarray.kind

  val elt_size_bytes : int
end

module type S = sig
  include S_in

  type t

  val create : int -> t

  val of_array : elt array -> t
end
