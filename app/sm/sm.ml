open Core.Std
open Async.Std
open Res_lib.Std
open Render_lib.Std
open Input_lib.Std

let run_render_test () =
  Renderer.with_renderer (fun renderer ->
    Renderer.test renderer)
;;

let run_render_mesh mesh_file program_file () =
  let load_res_exn file =
    let (`Dir dir, `Pack pack, `Name name) = Res_id.analyze_filename file in
    Res_db.add_pack ~dir;
    Res_db.load
      ~id:(Res_id.create ~pack ~name)
      ~cache_until:`Don't_cache
    |> Deferred.Or_error.ok_exn
  in
  load_res_exn mesh_file
  >>= fun mesh_res ->
  load_res_exn program_file
  >>= fun program_res ->
  Renderer.with_renderer (fun renderer ->
    let mesh =
      match Res.data mesh_res with
      | `Mesh mesh -> mesh
      | _          -> failwithf "Not a mesh: %s" mesh_file ()
    in
    let program =
      match Res.data program_res with
      | `Program program -> program
      | _                -> failwithf "Not a program: %s" program_file ()
    in
    let rec loop camera =
      Renderer.on_ui_thread renderer
        (Renderer.render_mesh renderer ~program ~mesh ~camera)
      >>= fun () ->
      Clock.after (sec 0.1)
      >>= fun () ->
      Renderer.on_ui_thread renderer
        (Renderer.with_sdl_window renderer Input.process_events)
      >>= fun snapshot ->
      let camera =
        let if_pressed key value ~d =
          if Input.Snapshot.pressed snapshot key
          then value +. d
          else value
        in
        let (x, y, z) = Camera.position camera in
        let z = if_pressed Key.up z ~d:(-. 0.1) in
        let z = if_pressed Key.down z ~d:0.1 in
        let x = if_pressed Key.left x ~d:(-. 0.1) in
        let x = if_pressed Key.right x ~d:0.1 in
        Camera.set_position camera (x, y, z)
      in
      if Input.Snapshot.pressed snapshot Key.quit
      then Deferred.unit
      else loop camera
    in
    let camera =
      (* CR scvalex: Why is this 2.2 and not 1.2? *)
      Camera.translate ~z:2.2
        (Camera.create ())
    in
    loop camera)
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
                     +> flag "mesh" (required file)
                          ~doc:"FILE res file for the mesh to render"
                     +> flag "program" (required file)
                          ~doc:"FILE res file for the shading programg to use")
                 run_render_mesh)
            ])
       ])
;;

let () = Exn.handle_uncaught ~exit:true main;;
