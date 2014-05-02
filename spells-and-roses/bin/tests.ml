open Core.Std

let simple_animation ~drawing_of_state =
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
      let drawing_of_state = drawing_of_state ~width ~height in
      Game.main_loop ~initial_state ~on_event ~on_step
        ~steps_per_sec ~drawing_of_state ~renderer)
;;

module Moving_rectangle = struct
  let run () =
    let drawing_of_state ~width ~height (`Step step) =
      Drawing.
        (centered_normalized_scene ~width ~height
           (translate
              ~x:(Float.of_int (step mod 75) /. 100.0)
              ~y:0.375
              (rectangle ~width:0.25 ~height:0.25)))
    in
    simple_animation ~drawing_of_state
  ;;
end

module Rectangles = struct
  let run () =
    let drawing_of_state ~width ~height _state =
      Drawing.Example.rectangles ~width ~height
    in
    simple_animation ~drawing_of_state
  ;;
end

module Static_text = struct
  type t = {
    direction    : [`Up | `Down] option;
    acceleration : float;
    position_y   : float;
  }

  let run ?(data_dir = "resources") () =
    let hamlet's_soliloquy =
      In_channel.with_file (data_dir ^/ "hamlet.txt") ~f:(fun ch ->
          In_channel.input_all ch)
    in
    Game.with_sdl ~f:(fun ~renderer ~width ~height ->
        let initial_state =
          {
            direction    = None;
            acceleration = 0.0;
            position_y   = 0.1;
          }
        in
        let on_event ~state ev =
          match ev with
          | Sdlevent.Quit _
          | Sdlevent.KeyUp {Sdlevent. keycode = Sdlkeycode.Q; _} ->
            `Quit
          | Sdlevent.KeyDown {Sdlevent. keycode = Sdlkeycode.Down; _} ->
            `Continue {state with direction = Some `Up; }
          | Sdlevent.KeyUp {Sdlevent. keycode = Sdlkeycode.Down; _} ->
            `Continue {state with direction = None; }
          | Sdlevent.KeyDown {Sdlevent. keycode = Sdlkeycode.Up; _} ->
            `Continue {state with direction = Some `Down; }
          | Sdlevent.KeyUp {Sdlevent. keycode = Sdlkeycode.Up; _} ->
            `Continue {state with direction = None; }
          | _ ->
            `Continue state
        in
        let on_step state =
          let acceleration =
            state.acceleration
            +. (match state.direction with
                | None ->
                  0.0 -. state.acceleration /. 4.0
                | Some `Up ->
                  0.0 -. 0.01
                | Some `Down ->
                  0.01)
          in
          let position_y = state.position_y +. acceleration in
          `Continue {state with acceleration; position_y; }
        in
        let steps_per_sec = 30.0 in
        let drawing_of_state state =
          Drawing.
            (centered_normalized_scene ~width ~height
               (translate ~x:0.5 ~y:state.position_y
                  (text ~font:"UbuntuMono-B.ttf" ~size_pt:32
                     ~position:(`X `Centre, `Y `Top)
                     hamlet's_soliloquy)))
        in
        Game.main_loop ~initial_state ~on_event ~on_step
          ~steps_per_sec ~drawing_of_state ~renderer)
  ;;
end
