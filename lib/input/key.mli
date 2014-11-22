open Core.Std

type t = private
  | Escape
  | Quit   (* the window wants to close *)

include Comparable.S with type t := t
include Sexpable.S with type t := t
include Stringable.S with type t := t

val escape : t

val quit : t
