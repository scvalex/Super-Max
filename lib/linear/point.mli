open Core.Std

include module type of Types_intf.Point

val create : float -> float -> float -> float -> t

val to_string : t -> string

val origin : t

