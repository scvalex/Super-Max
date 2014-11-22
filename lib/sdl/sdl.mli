(** https://wiki.libsdl.org/CategoryAPI *)

open Core_kernel.Std

type window

type gl_context

type event =
  [ `Quit
  | `Unknown of string
  ] with sexp

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

val gl_create_context : window -> gl_context

val gl_delete_context : gl_context -> unit

val gl_swap_window : window -> unit

val poll_event : unit -> event option
