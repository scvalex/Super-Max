open Core.Std
open Async.Std
open Res_lib.Std
open Render_lib.Std

let run_render_test () =
  Renderer.with_renderer (fun renderer ->
    Renderer.test renderer)
;;

let run_render_mesh file () =
  let dir = Filename.dirname file in
  Res_db.add_pack ~dir;
  let pack = Filename.dirname dir in
  let name = Filename.(chop_extension (basename file)) in
  Res_db.load
    ~id:(Res_db.Id.create ~pack ~name)
    ~cache_until:`Don't_cache
  |> Deferred.Or_error.ok_exn
  >>= fun res ->
  Renderer.with_renderer (fun renderer ->
    match Res.data res with
    | `Mesh mesh ->
      Renderer.on_ui_thread renderer
        (Renderer.render_mesh renderer mesh)
      >>= fun () ->
      Clock.after (sec 3.0)
    | _ ->
      failwithf "Not a mesh: %s" file ())
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
