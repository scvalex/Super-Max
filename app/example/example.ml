open Core.Std
open Async.Std

let main () =
  Command.run Commands.command
;;

let () = Exn.handle_uncaught ~exit:true main;;
