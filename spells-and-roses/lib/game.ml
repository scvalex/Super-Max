open Core.Std
open Async.Std

include Game_intf

module type Main = sig
  val main_loop :
       ctx : Drawing.Context.t
    -> width : int
    -> height : int
    -> unit Deferred.t
end

module Make(G : S) = struct
  module Broadcast_hub = Broadcast_hub.Make(G.Update.Query)(G.Update)(G.Update.Snapshot)

  (* CR scvalex: Actually broadcast updates. *)
  (** [main_loop] runs the game's event loop.  It tries to run at a
      fixed number of [steps_per_second] regardless of rendering time.
      Since the Async scheduler is running, it is possible to run Async
      jobs in the event handlers, but they are not allowed to block the
      event loop.  In other words, the event handlers may be impure but
      non-blocking. *)
  let main_loop ~ctx ~width ~height =
    Broadcast_hub.create ()
    >>= fun hub ->
    let slice = Float.iround_exn (1000.0 /. G.steps_per_second) in
    let game = G.create ~width ~height in
    don't_wait_for
      (Pipe.iter (Broadcast_hub.events hub) ~f:(fun _event ->
         (* CR scvalex: Pass events to game here. *)
         failwith "Not implemented"));
    let rec event_loop ~history ~step ~engine =
      match Sdlevent.poll_event () with
      | None ->
        `Continue (history, engine)
      | Some ev ->
        let history = (step, ev) :: history in
        G.on_event game ~engine ev;
        if Engine.Internal.quitting engine
        then `Quit (history, engine)
        else event_loop ~history ~step ~engine
    in
    let rec step_loop ~step ~game_ticks ~now ~engine =
      if Int.(game_ticks < now)
      then begin
        let game_ticks = game_ticks + slice in
        let step = step + 1 in
        G.on_step game ~engine;
        if Engine.Internal.quitting engine
        then `Quit (step, engine)
        else step_loop ~step ~game_ticks ~now ~engine
      end else begin
        `Continue (step, game_ticks, engine)
      end
    in
    let rec loop ~step ~history ~game_ticks ~skipped_frames =
      let engine = Engine.Internal.create () in
      match event_loop ~history ~step ~engine with
      | `Quit (history, _engine) ->
        Deferred.return (`Step step, history, `Skipped_frames skipped_frames)
      | `Continue (history, engine) ->
        let now = Sdltimer.get_ticks () in
        let old_step = step in
        match step_loop ~step ~game_ticks ~now ~engine with
        | `Quit (step, _engine) ->
          Deferred.return (`Step step, history, `Skipped_frames skipped_frames)
        | `Continue (step, game_ticks, engine) ->
          let drawing = G.to_drawing game in
          Broadcast_hub.broadcast_updates hub
            (Engine.Internal.drain_updates engine);
          Drawing.render drawing ~ctx
          >>= fun () ->
          let skipped_frames =
            skipped_frames + Int.max 0 (step - old_step - 1)
          in
          Clock.after (Time.Span.of_ms (Float.of_int (game_ticks - now)))
          >>= fun () ->
          loop ~step ~history ~game_ticks ~skipped_frames
    in
    loop
      ~step:0
      ~history:[]
      ~game_ticks:(Sdltimer.get_ticks ())
      ~skipped_frames:0
    >>| fun (`Step step, history, `Skipped_frames skipped_frames) ->
    Printf.eprintf "Game loop finished at step %d with history %d long\n%!"
      step (List.length history);
    Printf.eprintf "Skipped frames: %d (%.2f/s)\n%!"
      skipped_frames Float.(of_int skipped_frames
                            /. (of_int step /. G.steps_per_second));
  ;;
end

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
  let module M = (val (module Make(G) : Main) : Main) in
  with_sdl ~data_dir ~f:M.main_loop
;;
