open Core.Std

include Types_intf.Mat

let create ?(m00 = 0.0) ?(m01 = 0.0) ?(m02 = 0.0) ?(m03 = 0.0)
      ?(m10 = 0.0) ?(m11 = 0.0) ?(m12 = 0.0) ?(m13 = 0.0)
      ?(m20 = 0.0) ?(m21 = 0.0) ?(m22 = 0.0) ?(m23 = 0.0)
      ?(m30 = 0.0) ?(m31 = 0.0) ?(m32 = 0.0) ?(m33 = 0.0) () =
  { m00; m01; m02; m03;
    m10; m11; m12; m13;
    m20; m21; m22; m23;
    m30; m31; m32; m33; }
;;

let to_string t =
  sprintf "(%.2f %.2f %.2f %.2f)(%.2f %.2f %.2f %.2f)(%.2f %.2f %.2f %.2f)(%.2f %.2f %.2f %.2f)"
    t.m00 t.m01 t.m02 t.m03
    t.m10 t.m11 t.m12 t.m13
    t.m20 t.m21 t.m22 t.m23
    t.m30 t.m31 t.m32 t.m33
;;

let zero =
  create ()
;;

let id =
  create ~m00:1.0 ~m11:1.0 ~m22:1.0 ~m33:1.0 ()
;;

let of_rows r0 r1 r2 r3 =
  create
    ~m00:r0.(0) ~m01:r0.(1) ~m02:r0.(2) ~m03:r0.(3)
    ~m10:r1.(0) ~m11:r1.(1) ~m12:r1.(2) ~m13:r1.(3)
    ~m20:r2.(0) ~m21:r2.(1) ~m22:r2.(2) ~m23:r2.(3)
    ~m30:r3.(0) ~m31:r3.(1) ~m32:r3.(2) ~m33:r3.(3)
    ()
;;

let to_array t =
  let array = Bigarray.Array1.create Bigarray.Float32 Bigarray.c_layout 16 in
  array.{0} <- t.m00; array.{1} <- t.m10; array.{2} <- t.m20; array.{3} <- t.m30;
  array.{4} <- t.m01; array.{5} <- t.m11; array.{6} <- t.m21; array.{7} <- t.m31;
  array.{8} <- t.m02; array.{9} <- t.m12; array.{10} <- t.m22; array.{11} <- t.m32;
  array.{12} <- t.m03; array.{13} <- t.m13; array.{14} <- t.m23; array.{15} <- t.m33;
  array
;;

let perspective ~z_near ~z_far =
  create
    (* CR scvalex: Why does the cube seem elongated when I use z_near here? *)
    (* ~m00:(2.0 *. z_near) ~m11:(2.0 *. z_near) *)
    ~m00:1.0 ~m11:1.0
    ~m22:((z_near +. z_near) /. (z_near -. z_far))
    ~m23:(2.0 *. z_near *. z_far /. (z_near -. z_far))
    ~m32:(-. 1.0)
    ()
;;
