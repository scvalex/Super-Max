open Core.Std

type 'a resp = [`Continue of 'a | `Quit]

let main_loop ~initial_state ~on_event ~on_step ~steps_per_sec
    ~drawing_of_state ~renderer =
  let slice = Float.iround_exn (1000.0 /. steps_per_sec) in
  let rec event_loop ~state ~history ~step =
    match Sdlevent.poll_event () with
    | None ->
      `Continue (state, history)
    | Some ev ->
      let history = (step, ev) :: history in
      match on_event ~state ev with
      | `Quit ->
        `Quit history
      | `Continue state ->
        event_loop ~state ~history ~step
  in
  let rec step_loop ~state ~step ~game_ticks ~now =
    if Int.(game_ticks < now)
    then begin
      let game_ticks = game_ticks + slice in
      let step = step + 1 in
      match on_step state with
      | `Quit ->
        `Quit step
      | `Continue state ->
        step_loop ~state ~step ~game_ticks ~now
    end else begin
      `Continue (state, step, game_ticks)
    end
  in
  let rec loop ~state ~step ~history ~game_ticks =
    match event_loop ~state ~history ~step with
    | `Quit history ->
      (step, history)
    | `Continue (state, history) ->
      let now = Sdltimer.get_ticks () in
      match step_loop ~state ~step ~game_ticks ~now with
      | `Quit step ->
        (step, history)
      | `Continue (state, step, game_ticks) ->
        let drawing = drawing_of_state state in
        Drawing.render drawing ~renderer;
        Sdltimer.delay ~ms:(game_ticks - now);
        loop ~state ~step ~history ~game_ticks
  in
  let (step, history) =
    loop ~state:initial_state ~step:0 ~history:[] ~game_ticks:(Sdltimer.get_ticks ())
  in
  Printf.eprintf "Game loop finished at step %d with history %d long\n%!"
    step (List.length history);
;;

let with_sdl ~f =
  Sdl.init [`VIDEO];
  Sdlttf.init ();
  Sdlimage.init [`PNG];
  let window =
    Sdlwindow.create ~dims:(0, 0) ~pos:(`undefined, `undefined)
      ~title:"Something romantic"
      ~flags:[Sdlwindow.FullScreen_Desktop]
  in
  let renderer =
    Sdlrender.create_renderer ~win:window ~index:(0 - 1)
      ~flags:[Sdlrender.Accelerated; Sdlrender.PresentVSync]
  in
  let (width, height) = Sdlwindow.get_size window in
  Printf.eprintf "Window size is (%d, %d)\n" width height;
  Exn.protect
    ~f:(fun () -> f ~renderer ~width ~height)
    ~finally:(fun () ->
        Printf.eprintf "%s\n%!" (Drawing.Global.stats ());
        Sdlimage.quit ();
        Sdlttf.quit ();
        Sdl.quit ())
;;
