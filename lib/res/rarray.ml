open Core.Std let _ = _squelch_unused_module_warning_
open Rarray_intf

module Make(X : S_in) = struct
  open Bigarray.Array1

  include X

  type t = (elt, bigarray_elt, Bigarray.c_layout) Bigarray.Array1.t

  let create size =
    create bigarray_elt Bigarray.c_layout size
  ;;

  let of_array array =
    of_array bigarray_elt Bigarray.c_layout array
  ;;

  let length t =
    dim t
  ;;

  let size_bytes t =
    length t * elt_size_bytes
  ;;

  let iteri t ~f =
    for idx = 0 to length t - 1 do
      f idx t.{idx}
    done
  ;;
end

module Int = Make(struct
  type elt = int32
  type bigarray_elt = Bigarray.int32_elt

  let bigarray_elt = Bigarray.Int32;;

  let elt_size_bytes = 4;;
end)

module Float = Make(struct
  type elt = float
  type bigarray_elt = Bigarray.float32_elt

  let bigarray_elt = Bigarray.Float32;;

  let elt_size_bytes = 4;;
end)
