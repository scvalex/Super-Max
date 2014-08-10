open Core.Std
open Async.Std

include Game_intf

let main_loop ~initial_state ~on_event ~on_step ~steps_per_second
    ~to_drawing ~ctx =
  let slice = Float.iround_exn (1000.0 /. steps_per_second) in
  let rec event_loop ~state ~history ~step =
    match Sdlevent.poll_event () with
    | None ->
      `Continue (state, history)
    | Some ev ->
      let history = (step, ev) :: history in
      match on_event state ev with
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
  let rec loop ~state ~step ~history ~game_ticks ~skipped_frames =
    match event_loop ~state ~history ~step with
    | `Quit history ->
      Deferred.return (`Step step, history, `Skipped_frames skipped_frames)
    | `Continue (state, history) ->
      let now = Sdltimer.get_ticks () in
      let old_step = step in
      match step_loop ~state ~step ~game_ticks ~now with
      | `Quit step ->
        Deferred.return (`Step step, history, `Skipped_frames skipped_frames)
      | `Continue (state, step, game_ticks) ->
        let drawing = to_drawing state in
        Drawing.render drawing ~ctx
        >>= fun () ->
        let skipped_frames =
          skipped_frames + Int.max 0 (step - old_step - 1)
        in
        Clock.after (Time.Span.of_ms (Float.of_int (game_ticks - now)))
        >>= fun () ->
        loop ~state ~step ~history ~game_ticks ~skipped_frames
  in
  loop ~state:initial_state ~step:0 ~history:[] ~game_ticks:(Sdltimer.get_ticks ())
    ~skipped_frames:0
  >>| fun (`Step step, history, `Skipped_frames skipped_frames) ->
  Printf.eprintf "Game loop finished at step %d with history %d long\n%!"
    step (List.length history);
  Printf.eprintf "Skipped frames: %d (%.2f/s)\n%!"
    skipped_frames Float.(of_int skipped_frames /. (of_int step /. steps_per_second));
;;

let with_sdl ~f ~data_dir =
  let thread =
    Or_error.ok_exn
      (In_thread.Helper_thread.create ~name:"sdl-rendering-thread" ())
  in
  In_thread.run ~thread (fun () ->
      Sdl.init [`VIDEO];
      Sdlttf.init ();
      Sdlimage.init [`PNG; `JPG];
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
      (renderer, (`Width width, `Height height)))
  >>= fun (renderer, (`Width width, `Height height)) ->
  let ctx = Drawing.Context.create ~renderer ~thread ~data_dir in
  Printf.eprintf "Window size is (%d, %d)\n" width height;
  Monitor.protect (fun () -> f ~ctx ~width ~height)
    ~finally:(fun () ->
        Printf.eprintf "%s\n%!" (Drawing.Context.stats ctx);
        In_thread.run ~thread (fun () ->
            Sdlimage.quit ();
            Sdlttf.quit ();
            Sdl.quit ()))
;;

let run game ~data_dir =
  let module G = (val game : S) in
  with_sdl ~data_dir ~f:(fun ~ctx ~width ~height ->
      main_loop
        ~initial_state:(G.create ~width ~height)
        ~on_event:G.on_event
        ~on_step:G.on_step
        ~steps_per_second:G.steps_per_second
        ~to_drawing:G.to_drawing
        ~ctx)
;;
