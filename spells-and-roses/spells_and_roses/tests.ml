open Core.Std

module type Simple_animation_intf = sig
  val to_drawing :
       width : int
    -> height : int
    -> [`Step of int]
    -> Drawing.t
end

module No_multiplayer = struct
  module Update = struct
    module Query = struct
      type t = Nothing.t with bin_io, sexp
    end

    module Snapshot = struct
      type t = Nothing.t with bin_io, sexp
    end

    type t = Nothing.t with bin_io, sexp
  end

  let on_update_query _t ~engine:_ ~client_id:_ query =
    Nothing.unreachable_code query
  ;;

  let on_update _t ~engine:_ ~client_id:_ update =
    Nothing.unreachable_code update
  ;;
end

module Simple_animation(A : Simple_animation_intf) = struct
  include No_multiplayer

  type t = {
    width  : int;
    height : int;
    step   : int;
  }

  let create ~width ~height =
    let step = 0 in
    { width; height; step; }
  ;;

  let on_event t ~engine ev =
    match ev with
    | Sdlevent.Quit _
    | Sdlevent.KeyUp {Sdlevent. keycode = Sdlkeycode.Q; _} ->
      Engine.quit engine;
      t
    | _ ->
      t
  ;;

  let on_step t ~engine:_ =
    let t = { t with step = t.step + 1; } in
    t
  ;;

  let to_drawing t =
    A.to_drawing ~width:t.width ~height:t.height (`Step t.step)
  ;;

  let steps_per_second = 60.0;;
end

module Moving_rectangle = struct
  module Animation = Simple_animation(
    struct
      let to_drawing ~width ~height (`Step step) =
        Drawing.
          (centered_normalized_scene ~width ~height
             (translate
                ~x:(Float.of_int (step mod 75) /. 100.0)
                ~y:0.375
                (rectangle ~width:0.25 ~height:0.25 ~filled:true)))
      ;;
    end)

  let run ~data_dir =
    Game.run (module Animation) ~data_dir
  ;;
end

module Rectangles = struct
  module Animation = Simple_animation(
    struct
      let to_drawing ~width ~height _state =
        Drawing.Example.rectangles ~width ~height
      ;;
    end)

  let run ~data_dir =
    Game.run (module Animation) ~data_dir
  ;;
end

module Static_text = struct
  module type Args = sig
    val data_dir : string
  end

  module Animation(Args : Args) = struct
    include No_multiplayer

    type t = {
      direction          : [`Up | `Down] option;
      acceleration       : float;
      position_y         : float;
      hamlet's_soliloquy : string list;
      width              : int;
      height             : int;
    }

    let create ~width ~height =
      let hamlet's_soliloquy =
        In_channel.with_file (Args.data_dir ^/ "hamlet.txt") ~f:(fun ch ->
            String.split_lines (In_channel.input_all ch))
      in
      let direction = None in
      let acceleration = 0.0 in
      let position_y = 0.1 in
      { direction; acceleration; position_y;
        hamlet's_soliloquy; width; height; }
    ;;

    let on_event t ~engine ev =
      match ev with
      | Sdlevent.Quit _
      | Sdlevent.KeyUp {Sdlevent. keycode = Sdlkeycode.Q; _} ->
        Engine.quit engine;
        t
      | Sdlevent.KeyDown {Sdlevent. keycode = Sdlkeycode.Down; _}
      | Sdlevent.KeyDown {Sdlevent. keycode = Sdlkeycode.J; _} ->
        { t with direction = Some `Up; }
      | Sdlevent.KeyDown {Sdlevent. keycode = Sdlkeycode.Up; _}
      | Sdlevent.KeyDown {Sdlevent. keycode = Sdlkeycode.K; _}->
        { t with direction = Some `Down; }
      | Sdlevent.KeyUp _ ->
        { t with direction = None; }
      | _ ->
        t
    ;;

    let on_step t ~engine:_ =
      let acceleration =
        t.acceleration
        +. (match t.direction with
            | None ->
              0.0 -. t.acceleration /. 8.0
            | Some `Up ->
              0.0 -. 0.005
            | Some `Down ->
              0.005)
      in
      let position_y = t.position_y +. acceleration in
      { t with acceleration; position_y; }
    ;;

    let steps_per_second = 60.0;;

    let to_drawing t =
      Drawing.
        (centered_normalized_scene ~width:t.width ~height:t.height
           (translate ~x:0.5 ~y:t.position_y
              (text ~font:"UbuntuMono-B.ttf" ~size_pt:32
                 ~position:(`X `Centre, `Y `Top)
                 t.hamlet's_soliloquy)))
    ;;
  end

  let run ~data_dir =
    let module Args = (struct let data_dir = data_dir;; end) in
    Game.run (module Animation(Args)) ~data_dir
  ;;
end

module Dancing_banana = struct
  module Banana = struct
    let image = "banana.png";;
    let width = 320;;
    let height = 316;;
    let frames = 8;;
  end

  module Animation = Simple_animation(
    struct
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
            [ "IT'S PEANUT BUTTER"
            ; "JELLY TIME!!!"
            ]
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
      ;;
    end)

  let run ~data_dir =
    Game.run (module Animation) ~data_dir
  ;;
end

module Psy_cat = struct
  module Cat = struct
    let image = "psy_cat.jpg";;
    let width = 500;;
    let height = 485;;
  end

  module Animation = Simple_animation(
    struct
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
      ;;
    end)

  let run ~data_dir =
    Game.run (module Animation) ~data_dir
  ;;
end
