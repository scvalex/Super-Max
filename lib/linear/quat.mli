open Core.Std

include module type of Types_intf.Quat

val create :
  s : float
  -> float
  -> float
  -> float
  -> t

val of_vec : Vec.t -> t

val to_vec : t -> Vec.t

val to_string : t -> string

val scale : t -> float -> t

val conjugate : t -> t

val norm : t -> float

val norm_sq : t -> float

val inverse : t -> t

val ( + ) : t -> t -> t

val ( - ) : t -> t -> t

val ( * ) : t -> t -> t
