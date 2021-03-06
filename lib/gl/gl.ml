(** https://www.opengl.org/sdk/docs/man3/ *)

open Core.Std
open Ctypes
open Foreign
open Linear_lib.Std

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
    mutable live_vaos     : int;
  } with sexp

  let create () =
    { live_buffers = 0;
      live_shaders = 0;
      live_programs = 0;
      live_vaos = 0;
    }
  ;;
end

let stats = Stats.create ();;

module Gl = struct
  type shader = UInt32.t
  let shader_t = uint32_t

  type program = UInt32.t
  let program_t = uint32_t

  type buffer = UInt32.t
  let buffer_t = uint32_t

  let gl_enum_t = uint32_t

  type vertex_array_object = UInt32.t
  let vertex_array_t = uint32_t

  type uniform = Int32.t
  let uniform_t = int32_t

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

  let get_error () =
    let rec loop acc =
      match get_error () with
      | None     -> acc
      | Some err -> loop (err :: acc)
    in
    match loop [] with
    | []   -> None
    | errs -> Some (Error.of_list errs)
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

  let something_or_error something =
    let read x =
      match get_error () with
      | None     -> Ok x
      | Some err -> Error err
    in
    view ~read ~write:write_never something
  ;;

  let void_or_error =
    something_or_error void
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
      (uint32_t @-> int32_t @-> gl_enum_t @-> uchar @-> uint32_t @-> ptr void
       @-> returning void_or_error)
  ;;

  let draw_arrays =
    foreign "glDrawArrays"
      (gl_enum_t @-> int32_t @-> int32_t @-> returning void_or_error)
  ;;

  let gen_vertex_arrays =
    foreign "glGenVertexArrays"
      (uint32_t @-> ptr vertex_array_t @-> returning void_or_error)
  ;;

  let bind_vertex_array =
    foreign "glBindVertexArray" (vertex_array_t @-> returning void_or_error)
  ;;

  let draw_elements =
    foreign "glDrawElements"
      (gl_enum_t @-> int32_t @-> gl_enum_t @-> ptr void
       @-> returning void_or_error)
  ;;

  let enable =
    foreign "glEnable" (gl_enum_t @-> returning void_or_error)
  ;;

  let cull_face =
    foreign "glCullFace" (gl_enum_t @-> returning void_or_error)
  ;;

  let front_face =
    foreign "glFrontFace" (gl_enum_t @-> returning void_or_error)
  ;;

  let get_uniform_location =
    foreign "glGetUniformLocation"
      (program_t @-> string @-> returning (something_or_error uniform_t))
  ;;

  let uniform_1f =
    foreign "glUniform1f" (uniform_t @-> float @-> returning void_or_error)
  ;;

  let uniform_matrix_4fv =
    foreign "glUniformMatrix4fv"
      (uniform_t @-> uint32_t @-> uchar @-> ptr float @-> returning void_or_error)
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

module Draw_mode = struct
  type t = [ `Triangles ] with sexp

  let to_int = function
    | `Triangles -> 0x0004
  ;;
end

type draw_mode = Draw_mode.t with sexp

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
  let array_ptr = to_voidp (bigarray_start array1 (Rarray.data array)) in
  buffer_data
    (UInt32.of_int target)
    (Signed.Long.of_int (Rarray.size_bytes array))
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
    (Unsigned.UChar.of_int normalize)
    (UInt32.of_int stride)
    (to_voidp null)
  |> Or_error.ok_exn ~here:_here_
;;

let draw_arrays draw_mode ~first ~count =
  let draw_mode = Draw_mode.to_int draw_mode in
  draw_arrays (UInt32.of_int draw_mode) (Int32.of_int first) (Int32.of_int count)
  |> Or_error.ok_exn ~here:_here_
;;

let gen_vertex_array () =
  stats.Stats.live_vaos <- stats.Stats.live_vaos + 1;
  let vao_ptr = allocate uint32_t UInt32.zero in
  Or_error.ok_exn ~here:_here_ (gen_vertex_arrays (UInt32.of_int 1) vao_ptr);
  !@ vao_ptr
;;

let bind_vertex_array vao =
  bind_vertex_array vao
  |> Or_error.ok_exn ~here:_here_
;;

let draw_elements draw_mode ~indices:(`Bytes offset) ~count =
  let draw_mode = Draw_mode.to_int draw_mode in
  let type_ = 0x1405 in  (* GL_UNSINGED_INT *)
  let indices_ptr = ptr_of_raw_address (Int64.of_int offset) in
  draw_elements (UInt32.of_int draw_mode) (Int32.of_int count)
    (UInt32.of_int type_) indices_ptr
  |> Or_error.ok_exn ~here:_here_
;;

let enable cap =
  let cap =
    match cap with
    | `Cull_face -> 0x0B44
  in
  enable (UInt32.of_int cap)
  |> Or_error.ok_exn ~here:_here_
;;

let cull_face mode =
  let mode =
    match mode with
    | `Front          -> 0x0404
    | `Back           -> 0x0405
    | `Front_and_back -> 0x0408
  in
  cull_face (UInt32.of_int mode)
  |> Or_error.ok_exn ~here:_here_
;;

let front_face mode =
  let mode =
    match mode with
    | `Clockwise         -> 0x0900
    | `Counter_clockwise -> 0x0901
  in
  front_face (UInt32.of_int mode)
  |> Or_error.ok_exn ~here:_here_
;;

let get_uniform_location program name =
  let uniform =
    Or_error.ok_exn ~here:_here_
      (get_uniform_location program name)
  in
  if Int32.(uniform = of_int (-1))
  then failwithf "failed to get uniform location for %s" name ()
  else uniform
;;

let uniform uniform value =
  uniform_1f uniform value
  |> Or_error.ok_exn ~here:_here_
;;

let uniform_matrix uniform mat =
  let array_ptr = bigarray_start array1 (Mat.to_array mat) in
  uniform_matrix_4fv uniform (UInt32.of_int 1) (Unsigned.UChar.of_int 0) array_ptr
  |> Or_error.ok_exn ~here:_here_
;;

module Debug = struct
  let stats () =
    Sexp.to_string_mach (Stats.sexp_of_t stats)
  ;;
end
