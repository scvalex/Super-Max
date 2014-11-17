open Core_kernel.Std

(** https://www.opengl.org/sdk/docs/man3/ *)

type shader

type program

type buffer

val clear_color : float -> float -> float -> float -> unit

val clear :
  [ `Color_buffer_bit | `Depth_buffer_bit | `Stencil_buffer_bit ]
  -> unit

val create_shader :
  [ `Vertex_shader | `Geometry_shader | `Fragment_shader ]
  -> shader

val create_program : unit -> program

val gen_buffer : unit -> buffer

val bind_buffer :
  [ `Array_buffer | `Copy_read_buffer | `Copy_write_buffer | `Element_array_buffer
  | `Pixel_pack_buffer | `Pixel_unpack_buffer | `Texture_buffer
  | `Transform_feedback_buffer | `Uniform_buffer ]
  -> buffer option
  -> unit
