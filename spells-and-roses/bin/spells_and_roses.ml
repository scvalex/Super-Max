open Core.Std

let game_loop ~initial_state ~on_event ~on_step ~tps ~drawing_of_state ~renderer =
  let slice = Float.iround_exn (1000.0 /. tps) in
  let rec event_loop ~state ~history ~step =
    match Sdlevent.poll_event () with
    | None ->
      (state, history)
    | Some ev ->
      match on_event ~state ev with
      | `Quit ->
        Sdl.quit ();
        exit 0
      | `Continue state ->
        event_loop ~state ~history:((step, ev) :: history) ~step
  in
  let rec step_loop ~state ~step ~game_ticks ~now =
    if Int.(game_ticks < now)
    then begin
      let state = on_step state in
      step_loop ~state ~step:(step + 1) ~game_ticks:(game_ticks + slice) ~now
    end else begin
      (state, step, game_ticks)
    end
  in
  let rec loop ~state ~step ~history ~game_ticks =
    let (state, history) = event_loop ~state ~history ~step in
    let now = Sdltimer.get_ticks () in
    let (state, step, game_ticks) = step_loop ~state ~step ~game_ticks ~now in
    let drawing = drawing_of_state state in
    Drawing.render drawing ~renderer;
    Sdltimer.delay ~ms:(game_ticks - now);
    loop ~state ~step ~history ~game_ticks
  in
  loop ~state:initial_state ~step:0 ~history:[] ~game_ticks:(Sdltimer.get_ticks ())
;;

let run_game ~renderer ~width ~height =
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
    state
  in
  let tps = 30.0 in
  let drawing_of_state _state =
    Drawing.Example.rectangles ~width ~height
  in
  game_loop ~initial_state ~on_event ~on_step ~tps ~drawing_of_state ~renderer
;;

let run_test () =
  Sdl.init [`VIDEO];
  let (window, renderer) =
    Sdlrender.create_window_and_renderer
      ~width:0 ~height:0
      ~flags:[Sdlwindow.FullScreen_Desktop]
  in
  let (width, height) = Sdlwindow.get_size window in
  Printf.printf "Window size is (%d, %d)\n" width height;
  Sdlwindow.set_title ~window ~title:"Something romantic";
  run_game ~renderer ~width ~height
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
