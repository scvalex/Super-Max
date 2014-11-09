open Core_kernel.Std let _ = _squelch_unused_module_warning_
open Ctypes
open Foreign

(** https://www.opengl.org/sdk/docs/man3/ *)

module Gl = struct
  let clear_color =
    foreign "glClearColor" (float @-> float @-> float @-> float @-> returning void)
  ;;

  let clear =
    foreign "glClear" (uint32_t @-> returning void)
  ;;
end

include Gl

let clear what =
  let what =
    match what with
    | `Color_buffer_bit   -> 0x00004000
    | `Depth_buffer_bit   -> 0x00000100
    | `Stencil_buffer_bit -> 0x00000400
  in
  Gl.clear (Unsigned.UInt32.of_int what)
;;
