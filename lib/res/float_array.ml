open Core.Std let _ = _squelch_unused_module_warning_
open Bigarray.Array1

type t = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t

let create size =
  create Bigarray.Float32 Bigarray.c_layout size
;;

let of_array array =
  of_array Bigarray.Float32 Bigarray.c_layout array
;;

let length t =
  dim t
;;
