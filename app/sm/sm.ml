open Core.Std
open Async.Std
open Res_lib.Std
open Sdl_lib.Std
open Gl_lib.Std

let run_render_test () =
  Sdl.init ();
  Sdl.gl_set_attribute `Context_major_version 3;
  Sdl.gl_set_attribute `Context_major_version 3;
  Sdl.gl_set_attribute `Doublebuffer 1;
  Sdl.gl_set_attribute `Depthsize 24;
  Printf.printf "Let's rock\n%!";
  let window = Sdl.create_window ~title:"Rock" in
  let gl_context = Sdl.gl_create_context window in
  Gl.clear_color 0.1 0.1 0.1 1.0;
  Gl.clear `Color_buffer_bit;
  Sdl.gl_swap_window window;
  Sdl.delay ~ms:3000;
  Sdl.gl_delete_context gl_context;
  Sdl.destroy_window window;
  Sdl.quit ()
;;

let run_render_mesh file () =
  Res.load file
  |> Deferred.Or_error.ok_exn
  >>= fun _res ->
  Printf.printf "Rendring mesh\n%!";
  Deferred.unit
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
            ; ("mesh",
               Command.async
                 ~summary:"render a mesh"
                 Command.Spec.
                   ( empty
                     +> anon ("MESH" %: file) )
                 run_render_mesh)
            ])
       ])
;;

let () = Exn.handle_uncaught ~exit:true main;;
