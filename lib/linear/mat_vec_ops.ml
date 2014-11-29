open Core.Std let _ = _squelch_unused_module_warning_
open Types_intf

module V = Vec
module M = Mat

let ( |* ) v s =
  { Vec.
    x = v.V.x *. s;
    y = v.V.y *. s;
    z = v.V.z *. s;
    w = v.V.w *. s;
  }
;;

let ( *| ) s v =
  v |* s
;;

let ( ||*| ) m v =
  { Vec.
    x = m.M.m00 *. v.V.x +. m.M.m01 *. v.V.y +. m.M.m02 *. v.V.z +. m.M.m03 *. v.V.w;
    y = m.M.m10 *. v.V.x +. m.M.m11 *. v.V.y +. m.M.m12 *. v.V.z +. m.M.m13 *. v.V.w;
    z = m.M.m20 *. v.V.x +. m.M.m21 *. v.V.y +. m.M.m22 *. v.V.z +. m.M.m23 *. v.V.w;
    w = m.M.m30 *. v.V.x +. m.M.m31 *. v.V.y +. m.M.m32 *. v.V.z +. m.M.m33 *. v.V.w;
  }
;;

let ( |*|| ) =
  `Always_put_the_matrix_on_the_lhs
;;
