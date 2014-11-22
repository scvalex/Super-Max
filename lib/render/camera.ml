open Core.Std

type t = {
  position    : (float * float * float);
  orientation : (float * float * float);
} with sexp

let create () =
  let position = (0.0, 0.0, 0.0) in
  let orientation = (0.0, 0.0, 0.0) in
  { position; orientation; }
;;

let translate ?(x = 0.0) ?(y = 0.0) ?(z = 0.0) t =
  let _ = (x, y, z, t) in
  failwith "not implemented"
;;

let reorient ?(pitch = 0.0) ?(yaw = 0.0) ?(roll = 0.0) t =
  let _ = (pitch, yaw, roll, t) in
  failwith "not implemented"
;;
