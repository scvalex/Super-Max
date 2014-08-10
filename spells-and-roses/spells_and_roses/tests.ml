open Core.Std

let simple_animation ~to_drawing ~data_dir =
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
      let steps_per_second = 60.0 in
      let to_drawing = to_drawing ~width ~height in
      Game.main_loop ~initial_state ~on_event ~on_step
        ~steps_per_second ~to_drawing ~ctx)
;;

module Moving_rectangle = struct
  let run ~data_dir =
    let to_drawing ~width ~height (`Step step) =
      Drawing.
        (centered_normalized_scene ~width ~height
           (translate
              ~x:(Float.of_int (step mod 75) /. 100.0)
              ~y:0.375
              (rectangle ~width:0.25 ~height:0.25 ~filled:true)))
    in
    simple_animation ~to_drawing ~data_dir
  ;;
end

module Rectangles = struct
  let run ~data_dir =
    let to_drawing ~width ~height _state =
      Drawing.Example.rectangles ~width ~height
    in
    simple_animation ~to_drawing ~data_dir
  ;;
end

module Static_text = struct
  type t = {
    direction    : [`Up | `Down] option;
    acceleration : float;
    position_y   : float;
  }

  let run ~data_dir =
    let hamlet's_soliloquy =
      In_channel.with_file (data_dir ^/ "hamlet.txt") ~f:(fun ch ->
          String.split_lines (In_channel.input_all ch))
    in
    Game.with_sdl ~data_dir ~f:(fun ~ctx ~width ~height ->
        let initial_state =
          {
            direction    = None;
            acceleration = 0.0;
            position_y   = 0.1;
          }
        in
        let on_event state ev =
          match ev with
          | Sdlevent.Quit _
          | Sdlevent.KeyUp {Sdlevent. keycode = Sdlkeycode.Q; _} ->
            `Quit
          | Sdlevent.KeyDown {Sdlevent. keycode = Sdlkeycode.Down; _}
          | Sdlevent.KeyDown {Sdlevent. keycode = Sdlkeycode.J; _} ->
            `Continue {state with direction = Some `Up; }
          | Sdlevent.KeyDown {Sdlevent. keycode = Sdlkeycode.Up; _}
          | Sdlevent.KeyDown {Sdlevent. keycode = Sdlkeycode.K; _}->
            `Continue {state with direction = Some `Down; }
          | Sdlevent.KeyUp _ ->
            `Continue {state with direction = None; }
          | _ ->
            `Continue state
        in
        let on_step state =
          let acceleration =
            state.acceleration
            +. (match state.direction with
                | None ->
                  0.0 -. state.acceleration /. 8.0
                | Some `Up ->
                  0.0 -. 0.005
                | Some `Down ->
                  0.005)
          in
          let position_y = state.position_y +. acceleration in
          `Continue {state with acceleration; position_y; }
        in
        let steps_per_second = 60.0 in
        let to_drawing state =
          Drawing.
            (centered_normalized_scene ~width ~height
               (translate ~x:0.5 ~y:state.position_y
                  (text ~font:"UbuntuMono-B.ttf" ~size_pt:32
                     ~position:(`X `Centre, `Y `Top)
                     hamlet's_soliloquy)))
        in
        Game.main_loop ~initial_state ~on_event ~on_step
          ~steps_per_second ~to_drawing ~ctx)
  ;;
end

module Dancing_banana = struct
  module Banana = struct
    let image = "banana.png";;
    let width = 320;;
    let height = 316;;
    let frames = 8;;
  end

  let run ~data_dir =
    let to_drawing ~width ~height (`Step step) =
      let open Drawing in
      let banana =
        image Banana.image
          ~clip:(`X ((step / 6 mod Banana.frames) * Banana.width),
                 `Y 0,
                 `Width Banana.width,
                 `Height Banana.height)
      in
      let caption =
        text ~font:"UbuntuMono-B.ttf" ~size_pt:32
          ~position:(`X `Centre, `Y `Top)
          ["IT'S PEANUT BUTTER\nJELLY TIME!!!"]
      in
      (translate
         ~x:(Float.of_int (width - Banana.width) /. 2.0)
         ~y:(Float.of_int (height - Banana.height) /. 2.0 -. 50.0)
         (many
            [ banana
            ; translate
                ~x:(Float.of_int Banana.width /. 2.0)
                ~y:(Float.of_int Banana.height +. 30.0)
                caption
            ]))
    in
    simple_animation ~to_drawing ~data_dir
  ;;
end

module Psy_cat = struct
  module Cat = struct
    let image = "psy_cat.jpg";;
    let width = 500;;
    let height = 485;;
  end

  let run ~data_dir =
    let to_drawing ~width ~height (`Step step) =
      let ratio =
        Float.(max
                 (of_int width /. of_int Cat.width)
                 (of_int height /. of_int Cat.height))
      in
      Drawing.
        (translate
           ~x:Float.((of_int width -. of_int Cat.width *. ratio) /. 2.0)
           ~y:Float.((of_int height -. of_int Cat.height *. ratio) /. 2.0)
           (scale ~x:ratio ~y:ratio
              (image ~angle_deg:(Float.of_int step) Cat.image)))
    in
    simple_animation ~to_drawing ~data_dir
  ;;
end
