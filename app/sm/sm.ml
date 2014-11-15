open Core.Std
open Async.Std
open Res_lib.Std
open Render_lib.Std

let run_render_test () =
  Renderer.with_renderer (fun renderer ->
    Renderer.test renderer)
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
               Command.async
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
