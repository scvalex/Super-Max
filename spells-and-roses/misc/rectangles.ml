open Core.Std let _ = _squelch_unused_module_warning_
open Super_max.Std

let run () =
  let data_dir = "resources" in
  Game.with_sdl ~data_dir ~f:(fun ~ctx ~width ~height ->
      let initial_state = `Step 0 in
      let on_event state ev =
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
      let steps_per_sec = 60.0 in
      let drawing_of_state (`Step _step) =
        Drawing.Example.rectangles ~width ~height
      in
      Game.main_loop ~initial_state ~on_event ~on_step
        ~steps_per_sec ~drawing_of_state ~ctx)
;;
