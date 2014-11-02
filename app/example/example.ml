open Core.Std
open Async.Std

let example () =
  Printf.printf "Hello, Ocaml!\n%!";
  Deferred.unit
;;

let main () =
  Command.run
    (Command.async
       ~summary:"Example program"
       Command.Spec.empty
       example)
;;

let () = Exn.handle_uncaught ~exit:true main;;
