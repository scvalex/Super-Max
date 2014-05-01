open Core.Std

let run_test () =
  Game.with_sdl ~f:(fun ~renderer ~width ~height ->
    let initial_state = 0 in
    let on_event ~state ev =
      match ev with
      | Sdlevent.Quit _
      | Sdlevent.KeyUp {Sdlevent. keycode = Sdlkeycode.Q; _} ->
        `Quit
      | _ ->
        `Continue state
    in
    let on_step state =
      `Continue state
    in
    let steps_per_sec = 30.0 in
    let drawing_of_state _state =
      Drawing.Example.rectangles ~width ~height
    in
    Game.main_loop ~initial_state ~on_event ~on_step
      ~steps_per_sec ~drawing_of_state ~renderer)
;;

let main () =
  Command.run
    (Command.group ~summary:"Spells and Roses"
       [ ( "test",
           Command.basic
             ~summary:"system test"
             Command.Spec.
               ( empty )
             run_test )
       ])
;;

let () = Exn.handle_uncaught ~exit:true main;;
