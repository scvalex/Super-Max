open Core.Std
open Async.Std

let example () =
  Printf.printf "Hello, Ocaml!\n%!";
  Deferred.unit
;;

let command =
  Command.async
    ~summary:"Example program"
    Command.Spec.empty
    example
;;
