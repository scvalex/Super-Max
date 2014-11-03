open Core.Std

type mat3 = { m00 : float; m01 : float; m02 : float;
              m10 : float; m11 : float; m12 : float;
              m20 : float; m21 : float; m22 : float;
            }

type vec3 = { v0 : float; v1 : float; v2 : float; }

let id () =
  {
    m00 = 1.0; m01 = 0.0; m02 = 0.0;
    m10 = 0.0; m11 = 1.0; m12 = 0.0;
    m20 = 0.0; m21 = 0.0; m22 = 1.0;
  }
;;

let scale ~x ~y =
  {
    m00 = x;   m01 = 0.0; m02 = 0.0;
    m10 = 0.0; m11 = y;   m12 = 0.0;
    m20 = 0.0; m21 = 0.0; m22 = 1.0;
  }
;;

let translate ~x ~y =
  {
    m00 = 1.0; m01 = 0.0; m02 = x;
    m10 = 0.0; m11 = 1.0; m12 = y;
    m20 = 0.0; m21 = 0.0; m22 = 1.0;
  }
;;

let ( * ) m1 m2 =
  {
    m00 = m1.m00 *. m2.m00 +. m1.m01 *. m2.m10 +. m1.m02 *. m2.m20;
    m01 = m1.m00 *. m2.m01 +. m1.m01 *. m2.m11 +. m1.m02 *. m2.m21;
    m02 = m1.m00 *. m2.m02 +. m1.m01 *. m2.m12 +. m1.m02 *. m2.m22;
    m10 = m1.m10 *. m2.m00 +. m1.m11 *. m2.m10 +. m1.m12 *. m2.m20;
    m11 = m1.m10 *. m2.m01 +. m1.m11 *. m2.m11 +. m1.m12 *. m2.m21;
    m12 = m1.m10 *. m2.m02 +. m1.m11 *. m2.m12 +. m1.m12 *. m2.m22;
    m20 = m1.m20 *. m2.m00 +. m1.m21 *. m2.m10 +. m1.m22 *. m2.m20;
    m21 = m1.m20 *. m2.m01 +. m1.m21 *. m2.m11 +. m1.m22 *. m2.m21;
    m22 = m1.m20 *. m2.m02 +. m1.m21 *. m2.m12 +. m1.m22 *. m2.m22;
  }
;;

let ( *| ) mat vec =
  {
    v0 = mat.m00 *. vec.v0 +. mat.m01 *. vec.v1 +. mat.m02 *. vec.v2;
    v1 = mat.m10 *. vec.v0 +. mat.m11 *. vec.v1 +. mat.m12 *. vec.v2;
    v2 = mat.m20 *. vec.v0 +. mat.m21 *. vec.v1 +. mat.m22 *. vec.v2;
  }
;;

let vec3_of_xy ~x ~y =
  { v0 = x; v1 = y; v2 = 1.0; }
;;

let xy_of_vec3 vec =
  (vec.v0, vec.v1)
;;
