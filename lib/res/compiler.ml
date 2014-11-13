(* open Core.Std *)
(* open Async.Std *)

let compile ~dir =
  Resbuild.load ~dir
  >>= fun _resbuild ->
  failwith "not implemented"
;;
