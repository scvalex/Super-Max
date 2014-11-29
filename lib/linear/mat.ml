open Core.Std let _ = _squelch_unused_module_warning_

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
