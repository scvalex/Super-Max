open Core.Std

include module type of Types_intf.Quat

val create :
  s : float
  -> float
  -> float
  -> float
  -> t

val to_string : t -> string

val scale : t -> float -> t

val conjugate : t -> t

val norm : t -> float

val norm_sq : t -> float

val inverse : t -> t

val dot : t -> t -> float

module O : sig
  val ( + ) : t -> t -> t

  val ( - ) : t -> t -> t

  val ( * ) : t -> t -> t
end
