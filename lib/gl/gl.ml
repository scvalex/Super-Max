(** https://www.opengl.org/sdk/docs/man3/ *)

open Core_kernel.Std let _ = _squelch_unused_module_warning_
open Ctypes
open Foreign

module UInt32 = Unsigned.UInt32

module Gl = struct
  type shader = Unsigned.uint32
  let shader_t = uint32_t

  type program = Unsigned.uint32
  let program_t = uint32_t

  type buffer = Unsigned.uint32
  let buffer_t = uint32_t

  let gl_enum_t = uint32_t

  let write_never _ =
    failwith "write_never"
  ;;

  let get_error =
    foreign "glGetError" (void @-> returning uint32_t)
  ;;

  let get_error () =
    match UInt32.to_int (get_error ()) with
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

  let nonzero_uint32_t t =
    nonzero_to_ok ~to_int:UInt32.to_int t
  ;;

  let clear_color =
    foreign "glClearColor" (float @-> float @-> float @-> float @-> returning void)
  ;;

  let clear =
    foreign "glClear" (gl_enum_t @-> returning void_or_error)
  ;;

  let create_shader =
    foreign "glCreateShader" (gl_enum_t @-> returning (nonzero_uint32_t shader_t))
  ;;

  let create_program =
    foreign "glCreateProgram" (void @-> returning (nonzero_uint32_t program_t))
  ;;

  let gen_buffers =
    foreign "glGenBuffers" (uint32_t @-> ptr buffer_t @-> returning void_or_error)
  ;;

  let bind_buffer =
    foreign "glBindBuffer" (gl_enum_t @-> buffer_t @-> returning void_or_error)
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
  Gl.clear (UInt32.of_int what)
  |> Or_error.ok_exn
;;

let create_shader kind =
  let kind =
    match kind with
    | `Vertex_shader   -> 0x8B31
    | `Geometry_shader -> 0x8DD9
    | `Fragment_shader -> 0x8B30
  in
  create_shader (UInt32.of_int kind)
  |> Or_error.ok_exn
;;

let create_program () =
  create_program ()
  |> Or_error.ok_exn
;;

let gen_buffer () =
  let buffer_ptr = allocate uint32_t UInt32.zero in
  Or_error.ok_exn (gen_buffers (UInt32.of_int 1) buffer_ptr);
  !@ buffer_ptr
;;

let bind_buffer target buffer =
  let target =
    match target with
    | `Array_buffer              -> 0x8892
    | `Copy_read_buffer          -> 0x8F36
    | `Copy_write_buffer         -> 0x8F37
    | `Element_array_buffer      -> 0x8893
    | `Pixel_pack_buffer         -> 0x88EB
    | `Pixel_unpack_buffer       -> 0x88EC
    | `Texture_buffer            -> 0x8C2A
    | `Transform_feedback_buffer -> 0x8C8E
    | `Uniform_buffer            -> 0x8A11
  in
  let buffer =
    match buffer with
    | None   -> UInt32.zero
    | Some n -> n
  in
  bind_buffer (UInt32.of_int target) buffer
  |> Or_error.ok_exn
;;
