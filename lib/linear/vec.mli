open Core.Std

include module type of Types_intf.Vec

val create : float -> float -> float -> t

val to_string : t -> string

val zero : t

val norm : t -> float

val norm_sq : t -> float
