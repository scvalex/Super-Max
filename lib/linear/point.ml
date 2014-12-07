open Core.Std

include Types_intf.Point

let create x y z w =
  { x; y; z; w; }
;;

let to_string t =
  sprintf "(%.2f %.2f %.2f %.2f)" t.x t.y t.z t.w
;;

let origin =
  create 0.0 0.0 0.0 0.0
;;

let normalize t =
  if Float.(t.w = 1.0)
  then
    t
  else
    {
      x = t.x /. t.w;
      y = t.y /. t.w;
      z = t.z /. t.w;
      w = 1.0;
    }
;;
