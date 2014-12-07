open Core.Std let _ = _squelch_unused_module_warning_

module V = Vec
module M = Mat

let ( |* ) v s =
  { Vec.
    x = v.V.x *. s;
    y = v.V.y *. s;
    z = v.V.z *. s;
  }
;;

let ( *| ) s v =
  v |* s
;;

let ( ||*| ) m v =
  { Vec.
    x = m.M.m00 *. v.V.x +. m.M.m01 *. v.V.y +. m.M.m02 *. v.V.z;
    y = m.M.m10 *. v.V.x +. m.M.m11 *. v.V.y +. m.M.m12 *. v.V.z;
    z = m.M.m20 *. v.V.x +. m.M.m21 *. v.V.y +. m.M.m22 *. v.V.z;
  }
;;

let ( |*|| ) =
  `Always_put_the_matrix_on_the_lhs
;;

let ( ||*|| ) a b =
  let open Mat in
  { m00 = a.m00 *. b.m00 +. a.m01 *. b.m10 +. a.m02 *. b.m20 +. a.m03 *. b.m30
  ; m01 = a.m00 *. b.m01 +. a.m01 *. b.m11 +. a.m02 *. b.m21 +. a.m03 *. b.m31
  ; m02 = a.m00 *. b.m02 +. a.m01 *. b.m12 +. a.m02 *. b.m22 +. a.m03 *. b.m32
  ; m03 = a.m00 *. b.m03 +. a.m01 *. b.m13 +. a.m02 *. b.m23 +. a.m03 *. b.m33
  ; m10 = a.m10 *. b.m00 +. a.m11 *. b.m10 +. a.m12 *. b.m20 +. a.m13 *. b.m30
  ; m11 = a.m10 *. b.m01 +. a.m11 *. b.m11 +. a.m12 *. b.m21 +. a.m13 *. b.m31
  ; m12 = a.m10 *. b.m02 +. a.m11 *. b.m12 +. a.m12 *. b.m22 +. a.m13 *. b.m32
  ; m13 = a.m10 *. b.m03 +. a.m11 *. b.m13 +. a.m12 *. b.m23 +. a.m13 *. b.m33
  ; m20 = a.m20 *. b.m00 +. a.m21 *. b.m10 +. a.m22 *. b.m20 +. a.m23 *. b.m30
  ; m21 = a.m20 *. b.m01 +. a.m21 *. b.m11 +. a.m22 *. b.m21 +. a.m23 *. b.m31
  ; m22 = a.m20 *. b.m02 +. a.m21 *. b.m12 +. a.m22 *. b.m22 +. a.m23 *. b.m32
  ; m23 = a.m20 *. b.m03 +. a.m21 *. b.m13 +. a.m22 *. b.m23 +. a.m23 *. b.m33
  ; m30 = a.m30 *. b.m00 +. a.m31 *. b.m10 +. a.m32 *. b.m20 +. a.m33 *. b.m30
  ; m31 = a.m30 *. b.m01 +. a.m31 *. b.m11 +. a.m32 *. b.m21 +. a.m33 *. b.m31
  ; m32 = a.m30 *. b.m02 +. a.m31 *. b.m12 +. a.m32 *. b.m22 +. a.m33 *. b.m32
  ; m33 = a.m30 *. b.m03 +. a.m31 *. b.m13 +. a.m32 *. b.m23 +. a.m33 *. b.m33
  }
;;
