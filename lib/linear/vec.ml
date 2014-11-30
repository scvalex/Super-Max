open Core.Std let _ = _squelch_unused_module_warning_

include Types_intf.Vec

let create x y z w =
  { x; y; z; w; }
;;

let to_string t =
  sprintf "(%.2f %.2f %.2f %.2f)" t.x t.y t.z t.w
;;

let zero =
  create 0.0 0.0 0.0 0.0
;;
