open Core.Std

include module type of Types_intf.Vec

val create : float -> float -> float -> float -> t

val zero : t
