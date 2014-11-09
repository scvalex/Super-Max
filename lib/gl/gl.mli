open Core_kernel.Std

(** https://www.opengl.org/sdk/docs/man3/ *)

val clear_color : float -> float -> float -> float -> unit

val clear :
  [ `Color_buffer_bit | `Depth_buffer_bit | `Stencil_buffer_bit ]
  -> unit
