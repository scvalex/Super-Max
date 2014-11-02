open Core.Std

let example () =
  Printf.printf "Hello, Ocaml!\n%!"
;;

let main () =
  Command.run
    (Command.basic
       ~summary:"Example program"
       Command.Spec.empty
       example)
;;

let () = Exn.handle_uncaught_exn ~exit:true main;;
