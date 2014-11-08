open Core.Std
open Sdl_lib.Std

let run_render_test () =
  Sdl.init () |> Or_error.ok_exn
;;

let main () =
  Command.run
    (Command.group ~summary:"super-max tools"
       [ ("render",
          Command.group ~summary:"render tools"
            [ ("test",
               Command.basic
                 ~summary:"test the renderer"
                 Command.Spec.empty
                 run_render_test)
            ])
       ])
;;

let () = Exn.handle_uncaught ~exit:true main;;
