open Core.Std

include module type of Types_intf.Mat

val create :
  ?m00 : float -> ?m01 : float -> ?m02 : float -> ?m03 : float
  -> ?m10 : float -> ?m11 : float -> ?m12 : float -> ?m13 : float
  -> ?m20 : float -> ?m21 : float -> ?m22 : float -> ?m23 : float
  -> ?m30 : float -> ?m31 : float -> ?m32 : float -> ?m33 : float
  -> unit
  -> t

val zero : t

val id : t

val of_rows : float array -> float array -> float array -> float array -> t
