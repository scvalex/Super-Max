(** https://www.opengl.org/sdk/docs/man3/ *)

open Core_kernel.Std
open Res_lib.Std
open Ctypes
open Foreign

module UInt32 = Unsigned.UInt32

module Int32 = Signed.Int32

module Or_error = struct
  include Or_error

  let ok_exn ~here or_error =
    match or_error with
    | Ok x      -> x
    | Error err -> Error.raise (Error.tag err (Source_code_position.to_string here))
  ;;
end

module Stats = struct
  type t = {
    mutable live_buffers  : int;
    mutable live_shaders  : int;
    mutable live_programs : int;
  } with sexp

  let create () =
    { live_buffers = 0;
      live_shaders = 0;
      live_programs = 0;
    }
  ;;
end

let stats = Stats.create ();;

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

  let delete_shader =
    foreign "glDeleteShader" (shader_t @-> returning void_or_error)
  ;;

  let shader_source =
    foreign "glShaderSource"
      (shader_t @-> int32_t @-> ptr string @-> ptr int32_t @-> returning void_or_error)
  ;;

  let compile_shader =
    foreign "glCompileShader" (shader_t @-> returning void_or_error)
  ;;

  let get_shader_iv =
    foreign "glGetShaderiv"
      (shader_t @-> gl_enum_t @-> ptr int32_t @-> returning void_or_error)
  ;;

  let get_shader_info_log =
    foreign "glGetShaderInfoLog"
      (program_t @-> int32_t @-> ptr int32_t @-> ptr char @-> returning void_or_error)
  ;;

  let create_program =
    foreign "glCreateProgram" (void @-> returning (nonzero_uint32_t program_t))
  ;;

  let attach_shader =
    foreign "glAttachShader" (program_t @-> shader_t @-> returning void_or_error)
  ;;

  let detach_shader =
    foreign "glDetachShader" (program_t @-> shader_t @-> returning void_or_error)
  ;;

  let link_program =
    foreign "glLinkProgram" (program_t @-> returning void_or_error)
  ;;

  let get_program_iv =
    foreign "glGetProgramiv"
      (program_t @-> gl_enum_t @-> ptr int32_t @-> returning void_or_error)
  ;;

  let get_program_info_log =
    foreign "glGetProgramInfoLog"
      (program_t @-> int32_t @-> ptr int32_t @-> ptr char @-> returning void_or_error)
  ;;

  let gen_buffers =
    foreign "glGenBuffers" (uint32_t @-> ptr buffer_t @-> returning void_or_error)
  ;;

  let bind_buffer =
    foreign "glBindBuffer" (gl_enum_t @-> buffer_t @-> returning void_or_error)
  ;;

  let buffer_data =
    foreign "glBufferData"
      (gl_enum_t @-> long @-> ptr void @-> gl_enum_t @-> returning void_or_error)
  ;;

  let use_program =
    foreign "glUseProgram" (program_t @-> returning void_or_error)
  ;;

  let enable_vertex_attrib_array =
    foreign "glEnableVertexAttribArray" (uint32_t @-> returning void_or_error)
  ;;

  let disable_vertex_attrib_array =
    foreign "glDisableVertexAttribArray" (uint32_t @-> returning void_or_error)
  ;;

  let vertex_attrib_pointer =
    foreign "glVertexAttribPointer"
      (uint32_t @-> int32_t @-> gl_enum_t @-> int32_t @-> int32_t @-> ptr void
       @-> returning void_or_error)
  ;;

  let draw_arrays =
    foreign "glDrawArrays"
      (gl_enum_t @-> int32_t @-> int32_t @-> returning void_or_error)
  ;;
end

include Gl

type shader_type =
  [ `Vertex_shader
  | `Geometry_shader
  | `Fragment_shader
  ] with sexp

module Buffer_target = struct
  type t =
    [ `Array_buffer | `Copy_read_buffer | `Copy_write_buffer | `Element_array_buffer
    | `Pixel_pack_buffer | `Pixel_unpack_buffer | `Texture_buffer
    | `Transform_feedback_buffer | `Uniform_buffer
    ] with sexp

  let to_int = function
    | `Array_buffer              -> 0x8892
    | `Copy_read_buffer          -> 0x8F36
    | `Copy_write_buffer         -> 0x8F37
    | `Element_array_buffer      -> 0x8893
    | `Pixel_pack_buffer         -> 0x88EB
    | `Pixel_unpack_buffer       -> 0x88EC
    | `Texture_buffer            -> 0x8C2A
    | `Transform_feedback_buffer -> 0x8C8E
    | `Uniform_buffer            -> 0x8A11
  ;;
end

type buffer_target = Buffer_target.t with sexp

let clear what =
  let what =
    match what with
    | `Color_buffer_bit   -> 0x00004000
    | `Depth_buffer_bit   -> 0x00000100
    | `Stencil_buffer_bit -> 0x00000400
  in
  Gl.clear (UInt32.of_int what)
  |> Or_error.ok_exn ~here:_here_
;;

let create_shader kind =
  stats.Stats.live_shaders <- stats.Stats.live_shaders + 1;
  let kind =
    match kind with
    | `Vertex_shader   -> 0x8B31
    | `Geometry_shader -> 0x8DD9
    | `Fragment_shader -> 0x8B30
  in
  create_shader (UInt32.of_int kind)
  |> Or_error.ok_exn ~here:_here_
;;

let delete_shader shader =
  stats.Stats.live_shaders <- stats.Stats.live_shaders - 1;
  delete_shader shader
  |> Or_error.ok_exn ~here:_here_
;;

let shader_source shader code =
  let code_ptr =
    allocate string code
  in
  shader_source shader (Int32.of_int 1) code_ptr (from_voidp int32_t null)
  |> Or_error.ok_exn ~here:_here_
;;

let compile_shader shader =
  compile_shader shader
  |> Or_error.ok_exn ~here:_here_
;;

let get_shader_iv shader pname =
  let pname =
    match pname with
    | `Compile_status  -> 0x8B81
    | `Info_log_length -> 0x8B84
  in
  let param_ptr = allocate int32_t Int32.zero in
  Or_error.ok_exn ~here:_here_
    (get_shader_iv shader (UInt32.of_int pname) param_ptr);
  Int32.to_int (!@ param_ptr)
;;

let get_shader_info_log shader ~max_length =
  let info_log_ptr = allocate_n char ~count:(max_length + 1) in
  Or_error.ok_exn ~here:_here_
    (get_shader_info_log shader (Int32.of_int max_length)
       (from_voidp int32_t null) info_log_ptr);
  string_from_ptr info_log_ptr ~length:(max_length - 1)
;;

let create_program () =
  stats.Stats.live_programs <- stats.Stats.live_programs + 1;
  create_program ()
  |> Or_error.ok_exn ~here:_here_
;;

let attach_shader program shader =
  attach_shader program shader
  |> Or_error.ok_exn ~here:_here_
;;

let detach_shader program shader =
  detach_shader program shader
  |> Or_error.ok_exn ~here:_here_
;;

let link_program program =
  link_program program
  |> Or_error.ok_exn ~here:_here_
;;

let get_program_iv program pname =
  let pname =
    match pname with
    | `Link_status     -> 0x8B82
    | `Info_log_length -> 0x8B84
  in
  let param_ptr = allocate int32_t Int32.zero in
  Or_error.ok_exn ~here:_here_
    (get_program_iv program (UInt32.of_int pname) param_ptr);
  Int32.to_int (!@ param_ptr)
;;

let get_program_info_log program ~max_length =
  let info_log_ptr = allocate_n char ~count:(max_length + 1) in
  Or_error.ok_exn ~here:_here_
    (get_program_info_log program (Int32.of_int max_length)
       (from_voidp int32_t null) info_log_ptr);
  string_from_ptr info_log_ptr ~length:(max_length - 1)
;;

let gen_buffer () =
  stats.Stats.live_buffers <- stats.Stats.live_buffers + 1;
  let buffer_ptr = allocate uint32_t UInt32.zero in
  Or_error.ok_exn ~here:_here_ (gen_buffers (UInt32.of_int 1) buffer_ptr);
  !@ buffer_ptr
;;

let bind_buffer target buffer =
  let target = Buffer_target.to_int target in
  let buffer =
    match buffer with
    | None   -> UInt32.zero
    | Some n -> n
  in
  bind_buffer (UInt32.of_int target) buffer
  |> Or_error.ok_exn ~here:_here_
;;

let with_bound_buffer target buffer ~f =
  bind_buffer target (Some buffer);
  Exn.protect ~f
    ~finally:(fun () -> bind_buffer target None)
;;

let buffer_data target array usage =
  let target = Buffer_target.to_int target in
  let usage =
    match usage with
    | `Stream_draw  -> 0x88E0
    | `Stream_read  -> 0x88E1
    | `Stream_copy  -> 0x88E2
    | `Static_draw  -> 0x88E4
    | `Static_read  -> 0x88E5
    | `Static_copy  -> 0x88E6
    | `Dynamic_draw -> 0x88E8
    | `Dynamic_read -> 0x88E9
    | `Dynamic_copy -> 0x88EA
  in
  let array_ptr =
    to_voidp (bigarray_start array1 array)
  in
  buffer_data
    (UInt32.of_int target)
    (Signed.Long.of_int (Float_array.size_bytes array))
    array_ptr
    (UInt32.of_int usage)
  |> Or_error.ok_exn ~here:_here_
;;

let use_program program =
  let program =
    match program with
    | None   -> UInt32.zero
    | Some n -> n
  in
  use_program program
  |> Or_error.ok_exn ~here:_here_
;;

let with_used_program program ~f =
  use_program (Some program);
  Exn.protect ~f
    ~finally:(fun () -> use_program None)
;;

let enable_vertex_attrib_array idx =
  enable_vertex_attrib_array (UInt32.of_int idx)
  |> Or_error.ok_exn ~here:_here_
;;

let disable_vertex_attrib_array idx =
  disable_vertex_attrib_array (UInt32.of_int idx)
  |> Or_error.ok_exn ~here:_here_
;;

let with_vertex_attrib_array idx ~f =
  enable_vertex_attrib_array idx;
  Exn.protect ~f
    ~finally:(fun () -> disable_vertex_attrib_array idx)
;;

let vertex_attrib_pointer idx ~size kind ~normalize ~stride =
  let kind =
    match kind with
    | `Float -> 0x1406
  in
  let normalize =
    if normalize then 1 else 0
  in
  vertex_attrib_pointer
    (UInt32.of_int idx)
    (Int32.of_int size)
    (UInt32.of_int kind)
    (Int32.of_int normalize)
    (Int32.of_int stride)
    (from_voidp void null)
  |> Or_error.ok_exn ~here:_here_
;;

let draw_arrays kind ~first ~count =
  let kind =
    match kind with
    | `Triangles -> 0x0004
  in
  draw_arrays (UInt32.of_int kind) (Int32.of_int first) (Int32.of_int count)
  |> Or_error.ok_exn ~here:_here_
;;

module Debug = struct
  let stats () =
    Sexp.to_string_mach (Stats.sexp_of_t stats)
  ;;
end
