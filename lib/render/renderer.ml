open Core.Std
open Async.Std
open Sdl_lib.Std
open Gl_lib.Std

type t = {
  thread : In_thread.Helper_thread.t;
  window : Sdl.window;
}

let on_ui_thread t f =
  In_thread.run ~thread:t.thread f
;;

let with_renderer f =
  let thread =
    Or_error.ok_exn
      (In_thread.Helper_thread.create ~name:"renderer" ())
  in
  In_thread.run ~thread (fun () ->
    Sdl.init ();
    Sdl.gl_set_attribute `Context_major_version 3;
    Sdl.gl_set_attribute `Context_major_version 3;
    Sdl.gl_set_attribute `Doublebuffer 1;
    Sdl.gl_set_attribute `Depthsize 24;
    let window = Sdl.create_window ~title:"Rock" in
    let gl_context = Sdl.gl_create_context window in
    (window, gl_context))
  >>= fun (window, gl_context) ->
  let t = { window; thread; } in
  Monitor.protect (fun () -> f t)
    ~finally:(fun () ->
      on_ui_thread t (fun () ->
        Sdl.gl_delete_context gl_context;
        Sdl.destroy_window window;
        Sdl.quit ()))
;;

let test t =
  on_ui_thread t (fun () ->
    Gl.clear_color 0.1 0.1 0.1 1.0;
    Gl.clear `Color_buffer_bit;
    Sdl.gl_swap_window t.window)
  >>= fun () ->
  Clock.after (sec 3.0)
;;
