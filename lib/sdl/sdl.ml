(* https://wiki.libsdl.org/CategoryAPI *)

open Core_kernel.Std
open Ctypes
open Foreign

module Sdl = struct
  type window = unit ptr
  let window : window typ = ptr void
  let window_opt : window option typ = ptr_opt void

  type gl_context = unit ptr
  let gl_context : gl_context typ = ptr void

  let write_never _ =
    failwith "write_never"
  ;;

  let get_error () =
    Error.of_string (foreign "SDL_GetError" (void @-> returning string) ())
  ;;

  let zero_to_ok =
    let read = function
      | 0   -> Ok ()
      | err -> Error (get_error ())
    in
    view ~read ~write:write_never int
  ;;

  let some_to_ok t =
    let read = function
      | Some v -> Ok v
      | None   -> Error (get_error ())
    in
    view ~read ~write:write_never t
  ;;

  let init =
    foreign "SDL_Init" (uint32_t @-> returning zero_to_ok)
  ;;

  let quit =
    foreign "SDL_Quit" (void @-> returning void)
  ;;

  let gl_set_attribute =
    foreign "SDL_GL_SetAttribute" (int @-> int @-> returning zero_to_ok)
  ;;

  let create_window =
    foreign "SDL_CreateWindow"
      (string
       @-> int @-> int @-> int @-> int
       @-> uint32_t
       @-> returning (some_to_ok window_opt))
  ;;

  let destroy_window =
    foreign "SDL_DestroyWindow" (window @-> returning void)
  ;;

  let delay =
    foreign "SDL_Delay" (uint32_t @-> returning void)
  ;;

  let gl_create_context =
    foreign "SDL_GL_CreateContext" (window @-> returning gl_context)
  ;;

  let gl_delete_context =
    foreign "SDL_GL_DeleteContext" (gl_context @-> returning void)
  ;;

  let gl_swap_window =
    foreign "SDL_GL_SwapWindow" (window @-> returning void)
  ;;
end

type window = Sdl.window

type gl_context = Sdl.gl_context

let init () =
  let audio = 0x00000010 in
  let video = 0x00000020 in
  let events = 0x00004000 in
  Sdl.init (Unsigned.UInt32.of_int (audio lor video lor events))
  |> Or_error.ok_exn
;;

let quit () =
  Sdl.quit ()
;;

let gl_set_attribute attr value =
  let attr =
    match attr with
    | `Doublebuffer          -> 5
    | `Depthsize             -> 6
    | `Context_major_version -> 17
    | `Context_minor_version -> 18
  in
  Sdl.gl_set_attribute attr value
  |> Or_error.ok_exn
;;

let create_window ~title =
  let opengl = 0x00000002 in
  let fullscreen_desktop = 0x00001001 in
  Sdl.create_window title 0 0 0 0
    (Unsigned.UInt32.of_int (opengl lor fullscreen_desktop))
  |> Or_error.ok_exn
;;

let destroy_window window =
  Sdl.destroy_window window
;;

let delay ~ms =
  Sdl.delay (Unsigned.UInt32.of_int ms)
;;

let gl_create_context window =
  Sdl.gl_create_context window
;;

let gl_delete_context gl_context =
  Sdl.gl_delete_context gl_context
;;

let gl_swap_window window =
  Sdl.gl_swap_window window
;;
