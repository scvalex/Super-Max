open Core.Std

module Vector2 : sig
  type t

  include Comparable.S with type t := t
  include Sexpable.S with type t := t
  include Stringable.S with type t := t

  val create :
    x : float
    -> y : float
    -> t

  val x : t -> float

  val y : t -> float
end

module Vector3 : sig
  type t

  include Comparable.S with type t := t
  include Sexpable.S with type t := t
  include Stringable.S with type t := t

  val create :
    x : float
    -> y : float
    -> z : float
    -> t

  val x : t -> float

  val y : t -> float

  val z : t -> float

  val xyz : t -> (float * float * float)
end

module Point2 : module type of Vector2

module Point3 : module type of Vector3

module Color3 : sig
  type t

  include Comparable.S with type t := t
  include Sexpable.S with type t := t
  include Stringable.S with type t := t

  val create :
    r : float
    -> g : float
    -> b : float
    -> t

  val r : t -> float

  val g : t -> float

  val b : t -> float

  val rgb : t -> (float * float * float)
end

module Radiance3 : module type of Color3

module Power3 : module type of Color3

module Ray : sig
  type t

  include Comparable.S with type t := t
  include Sexpable.S with type t := t
  include Stringable.S with type t := t

  val create :
    origin : Point3.t
    -> direction : Vector3.t
    -> t

  val origin : t -> Point3.t

  val direction : t -> Vector3.t
end
