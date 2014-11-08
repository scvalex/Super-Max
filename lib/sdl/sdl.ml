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
end

let init () =
  Sdl.init (Unsigned.UInt32.of_int 0)
;;
