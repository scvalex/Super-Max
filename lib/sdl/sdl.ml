(* https://wiki.libsdl.org/CategoryAPI *)

open Core_kernel.Std
open Ctypes
open Foreign

module Sdl = struct
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

  let init =
    foreign "SDL_Init" (uint32_t @-> returning zero_to_ok)
  ;;

  let quit =
    foreign "SDL_Quit" (void @-> returning void)
  ;;

  let gl_set_attribute =
    foreign "SDL_GL_SetAttribute" (int @-> int @-> returning zero_to_ok)
  ;;
end

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
