open Core_kernel.Std
open Res_lib.Std

(** https://www.opengl.org/sdk/docs/man3/ *)

type shader

type shader_type =
  [ `Vertex_shader
  | `Geometry_shader
  | `Fragment_shader
  ] with sexp

type program

type buffer

type buffer_target =
  [ `Array_buffer | `Copy_read_buffer | `Copy_write_buffer | `Element_array_buffer
  | `Pixel_pack_buffer | `Pixel_unpack_buffer | `Texture_buffer
  | `Transform_feedback_buffer | `Uniform_buffer
  ] with sexp

val clear_color : float -> float -> float -> float -> unit

val clear :
  [ `Color_buffer_bit | `Depth_buffer_bit | `Stencil_buffer_bit ]
  -> unit

val create_shader : shader_type -> shader

val delete_shader : shader -> unit

val shader_source : shader -> string -> unit

val compile_shader : shader -> unit

val get_shader_iv :
  shader
  -> [ `Compile_status | `Info_log_length ]
  -> int

val get_shader_info_log :
  shader
  -> max_length : int
  -> string

val create_program : unit -> program

val attach_shader : program -> shader -> unit

val detach_shader : program -> shader -> unit

val link_program : program -> unit

val get_program_iv :
  program
  -> [ `Link_status | `Info_log_length ]
  -> int

val get_program_info_log :
  program
  -> max_length : int
  -> string

val gen_buffer : unit -> buffer

val buffer_data :
  buffer_target
  -> Float_array.t
  -> [ `Stream_draw | `Stream_read | `Stream_copy
     | `Static_draw | `Static_read | `Static_copy
     | `Dynamic_draw | `Dynamic_read | `Dynamic_copy ]
  -> unit

val with_bound_buffer :
  buffer_target
  -> buffer
  -> f : (unit -> 'a)
  -> 'a

val with_used_program :
  program
  -> f : (unit -> 'a)
  -> 'a

val with_vertex_attrib_array :
  int
  -> f : (unit -> 'a)
  -> 'a

val vertex_attrib_pointer :
  int
  -> size : int
  -> [ `Float ]
  -> normalize : bool
  -> stride : int
  -> unit

val draw_arrays :
  [ `Triangles ]
  -> first : int
  -> count : int
  -> unit

module Debug : sig
  val stats : unit -> string
end
