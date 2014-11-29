open Core.Std let _ = _squelch_unused_module_warning_
open Rarray_intf
open Bigarray.Array1

type ('elt, 'bigarray_elt) t = {
  data           : ('elt, 'bigarray_elt, Bigarray.c_layout) Bigarray.Array1.t;
  elt_size_bytes : int;
} with fields

let length t =
  dim t.data
;;

let iteri t ~f =
  for idx = 0 to length t - 1 do
    f idx t.data.{idx}
  done
;;

let size_bytes t =
  length t * t.elt_size_bytes
;;

module Make(X : S_in) = struct
  include X

  type nonrec t = (elt, bigarray_elt) t

  let create size =
    let data = create bigarray_elt Bigarray.c_layout size in
    { data; elt_size_bytes; }
  ;;

  let of_array array =
    let data = of_array bigarray_elt Bigarray.c_layout array in
    { data; elt_size_bytes; }
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
