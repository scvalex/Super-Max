(** https://wiki.libsdl.org/CategoryAPI *)

open Core_kernel.Std

type window

val init : unit -> unit

val quit : unit -> unit

val gl_set_attribute :
  [ `Context_major_version | `Context_minor_version
  | `Depthsize | `Doublebuffer ]
  -> int
  -> unit

val create_window :
  title : string
  -> window

val destroy_window : window -> unit

val delay :
  ms : int
  -> unit
