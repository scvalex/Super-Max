open Core.Std let _ = _squelch_unused_module_warning_

include Types_intf.Vec

let create x y z =
  { x; y; z; }
;;

let to_string t =
  sprintf "(%.2f %.2f %.2f)" t.x t.y t.z
;;

let zero =
  create 0.0 0.0 0.0
;;

let norm_sq t =
  t.x *. t.x +. t.y *. t.y +. t.z *. t.z
;;

let norm t =
  sqrt (norm_sq t)
;;
