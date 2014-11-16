open Core_kernel.Std

(** https://www.opengl.org/sdk/docs/man3/ *)

type shader

type program

val clear_color : float -> float -> float -> float -> unit

val clear :
  [ `Color_buffer_bit | `Depth_buffer_bit | `Stencil_buffer_bit ]
  -> unit

val create_shader :
  [ `Vertex_shader | `Geometry_shader | `Fragment_shader ]
  -> shader

val create_program : unit -> program
