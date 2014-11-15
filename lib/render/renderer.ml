open Core.Std
open Async.Std
open Sdl_lib.Std
open Gl_lib.Std

type t = {
  window : Sdl.window;
}

let with_renderer f =
  Sdl.init ();
  Sdl.gl_set_attribute `Context_major_version 3;
  Sdl.gl_set_attribute `Context_major_version 3;
  Sdl.gl_set_attribute `Doublebuffer 1;
  Sdl.gl_set_attribute `Depthsize 24;
  let window = Sdl.create_window ~title:"Rock" in
  let gl_context = Sdl.gl_create_context window in
  let t = { window; } in
  f t
  >>| fun () ->
  Sdl.gl_delete_context gl_context;
  Sdl.destroy_window window;
  Sdl.quit ()
;;

let test t =
  Gl.clear_color 0.1 0.1 0.1 1.0;
  Gl.clear `Color_buffer_bit;
  Sdl.gl_swap_window t.window;
  Clock.after (sec 3.0)
;;
