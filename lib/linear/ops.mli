open Core.Std

val ( |* ) : Vec.t -> float -> Vec.t

val ( *| ) : float -> Vec.t -> Vec.t

val ( ||*| ) : Mat.t -> Vec.t -> Vec.t

val ( |*|| ) : [`Always_put_the_matrix_on_the_lhs]

val ( ||*|| ) : Mat.t -> Mat.t -> Mat.t

val ( .-. ) : Point.t -> Point.t -> Vec.t

val ( .+ ) : Point.t -> Vec.t -> Point.t

val ( |*% ) : Vec.t -> Quat.t -> Vec.t

val ( %*| ) : Quat.t -> Vec.t -> Vec.t

val mix : Point.t -> Point.t -> float -> Point.t

val dot : Vec.t -> Vec.t -> float

val rotation :
  about : Vec.t
  -> rad : float
  -> Quat.t
