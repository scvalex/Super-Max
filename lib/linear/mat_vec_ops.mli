open Core.Std

val ( |* ) : Vec.t -> float -> Vec.t

val ( *| ) : float -> Vec.t -> Vec.t

val ( ||*| ) : Mat.t -> Vec.t -> Vec.t

val ( |*|| ) : [`Always_put_the_matrix_on_the_lhs]
