open Core.Std

type t = private
  | Escape

include Comparable.S with type t := t
include Sexpable.S with type t := t
include Stringable.S with type t := t

val escape : t
