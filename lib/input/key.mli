open Core.Std
open Sdl_lib.Std

type t = private
  | Escape
  | Quit   (* the window wants to close *)
  | Down | Up | Left | Right

include Comparable.S with type t := t
include Sexpable.S with type t := t
include Stringable.S with type t := t

val escape : t

val quit : t

val of_sdl_key : Sdl.key -> t option
