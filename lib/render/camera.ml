open Core.Std
open Linear_lib.Std

type t = {
  position    : (float * float * float);
  orientation : (float * float * float);
} with fields, sexp

let create () =
  let position = (0.0, 0.0, 0.0) in
  let orientation = (0.0, 0.0, 0.0) in
  { position; orientation; }
;;

let set_position t position =
  { t with position; }
;;

let translate ?(x = 0.0) ?(y = 0.0) ?(z = 0.0) t =
  let position =
    let (x', y', z') = t.position in
    (x +. x', y +. y', z +. z')
  in
  { t with position; }
;;

let reorient ?(pitch = 0.0) ?(yaw = 0.0) ?(roll = 0.0) t =
  let _ = (pitch, yaw, roll, t) in
  failwith "not implemented"
;;

let transformation t =
  let (x, y, z) = t.position in
  Mat.create
    ~m00:1.0 ~m11:1.0 ~m22:1.0 ~m33:1.0
    ~m03:(-. x) ~m13:(-. y) ~m23:(-. z)
    ()
;;
