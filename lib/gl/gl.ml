open Core_kernel.Std let _ = _squelch_unused_module_warning_
open Ctypes
open Foreign

(** https://www.opengl.org/sdk/docs/man3/ *)

module Gl = struct
  type shader = Unsigned.uint32

  type program = Unsigned.uint32

  let write_never _ =
    failwith "write_never"
  ;;

  let get_error =
    foreign "glGetError" (void @-> returning uint32_t)
  ;;

  let get_error () =
    match Unsigned.UInt32.to_int (get_error ()) with
    | 0      -> None
    | 0x0500 -> Some (Error.createf "GL invalid enum")
    | 0x0501 -> Some (Error.createf "GL invalid value")
    | 0x0502 -> Some (Error.createf "GL invalid operation")
    | 0x0505 -> Some (Error.createf "GL out of memory")
    | 0x0506 -> Some (Error.createf "GL invalid framebuffer operation")
    | n      -> Some (Error.createf "GL error %d" n)
  ;;

  let nonzero_to_ok ~to_int t =
    let read n =
      if Int.(<>) (to_int n) 0
      then
        Ok n
      else
        match get_error () with
        | None     -> Error (Error.createf "function returned zero but no error was set")
        | Some err -> Error err
    in
    view ~read ~write:write_never t
  ;;

  let void_or_error =
    let read () =
      match get_error () with
      | None     -> Ok ()
      | Some err -> Error err
    in
    view ~read ~write:write_never void
  ;;

  let nonzero_uint32_t =
    nonzero_to_ok ~to_int:Unsigned.UInt32.to_int uint32_t
  ;;

  let clear_color =
    foreign "glClearColor" (float @-> float @-> float @-> float @-> returning void)
  ;;

  let clear =
    foreign "glClear" (uint32_t @-> returning void_or_error)
  ;;

  let create_shader =
    foreign "glCreateShader" (uint32_t @-> returning nonzero_uint32_t)
  ;;

  let create_program =
    foreign "glCreateProgram" (void @-> returning nonzero_uint32_t)
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
  |> Or_error.ok_exn
;;

let create_shader kind =
  let kind =
    match kind with
    | `Vertex_shader   -> 0x8B31
    | `Geometry_shader -> 0x8DD9
    | `Fragment_shader -> 0x8B30
  in
  create_shader (Unsigned.UInt32.of_int kind)
  |> Or_error.ok_exn
;;

let create_program () =
  create_program ()
  |> Or_error.ok_exn
;;
