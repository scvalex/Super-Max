open Core.Std

let run_test () =
  Game.with_sdl ~f:(fun ~renderer ~width ~height ->
    let initial_state = `Step 0 in
    let on_event ~state ev =
      match ev with
      | Sdlevent.Quit _
      | Sdlevent.KeyUp {Sdlevent. keycode = Sdlkeycode.Q; _} ->
        `Quit
      | _ ->
        `Continue state
    in
    let on_step (`Step step) =
      `Continue (`Step (step + 1))
    in
    let steps_per_sec = 30.0 in
    let drawing_of_state (`Step step) =
      Drawing.
        (centered_normalized_scene ~width ~height
           (translate
              ~x:(Float.of_int (step mod 75) /. 100.0)
              ~y:0.375
              (rectangle ~width:0.25 ~height:0.25)))
    in
    Game.main_loop ~initial_state ~on_event ~on_step
      ~steps_per_sec ~drawing_of_state ~renderer)
;;

let main () =
  Command.run
    (Command.group ~summary:"Spells and Roses"
       [ ( "test",
           Command.group ~summary:"System tests"
             [ ( "moving-rectangle",
                 Command.basic
                   ~summary:"Display a white rectangle moving horizontally"
                   Command.Spec.
                     ( empty )
                   run_test )
             ])
       ])
;;

let () = Exn.handle_uncaught ~exit:true main;;
