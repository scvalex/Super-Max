open Core.Std

type t

include Comparable.S with type t := t
include Hashable.S with type t := t

val create :
  pack : string
  -> name : string
  -> t

val to_string : t -> string

val name : t -> string

val pack  : t -> string

val filename : t -> string

val analyze_filename :
  string
  -> ([`Dir of string] * [`Pack of string] * [`Name of string])
