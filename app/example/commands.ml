open Core.Std
open Async.Std
open Linear_lib.Std

let example () =
  Printf.printf "Hello, Ocaml!\n%!";
  let _ = Mat.id in
  Deferred.unit
;;

let command =
  Command.async
    ~summary:"Example program"
    Command.Spec.empty
    example
;;
