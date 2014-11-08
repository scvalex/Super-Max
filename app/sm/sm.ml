open Core.Std
open Sdl_lib.Std

let run_render_test () =
  Sdl.init ();
  Sdl.gl_set_attribute `Context_major_version 3;
  Sdl.gl_set_attribute `Context_major_version 3;
  Sdl.gl_set_attribute `Doublebuffer 1;
  Sdl.gl_set_attribute `Depthsize 24;
  Printf.printf "Let's rock\n%!";
  let window = Sdl.create_window ~title:"Rock" in
  Sdl.delay 3000;
  Sdl.destroy_window window;
  Sdl.quit ()
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
