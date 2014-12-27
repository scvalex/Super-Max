open Core.Std
open Linear_lib.Std

type t = {
  position    : Point.t;
  orientation : (float * float * float);
} with fields, sexp

let create () =
  let position = Point.origin in
  let orientation = (0.0, 0.0, 0.0) in
  { position; orientation; }
;;

let translate ?x ?y ?z t =
  let position = Point.translate t.position ?x ?y ?z in
  { t with position; }
;;

let reorient ?(pitch = 0.0) ?(yaw = 0.0) ?(roll = 0.0) t =
  let _ = (pitch, yaw, roll, t) in
  failwith "not implemented"
;;

let transformation t =
  let p = Point.normalize t.position in
  Mat.create
    ~m00:1.0 ~m11:1.0 ~m22:1.0 ~m33:1.0
    ~m03:(-. p.Point.x) ~m13:(-. p.Point.y) ~m23:(-. p.Point.z)
    ()
;;
