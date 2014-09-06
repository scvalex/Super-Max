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

  val scale : t -> float -> t

  val magnitude : t -> float

  val direction : t -> t

  val (+) : t -> t -> t
end

module Point2 : module type of Vector2

module Point3 : sig
  include module type of Vector3

  val of_vector : Vector3.t -> t
end

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

  val of_vector : Vector3.t -> t
end

module Radiance3 : sig
  include module type of Color3

  val of_color : Color3.t -> t
end

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

module Bsdf : sig
  type t

  val test : t
end

module Triangle : sig
  type t

  include Comparable.S with type t := t
  include Sexpable.S with type t := t
  include Stringable.S with type t := t

  val create :
    vertices : (Point3.t * Point3.t * Point3.t)
    -> normals : (Vector3.t * Vector3.t * Vector3.t)
    -> bsdf : Bsdf.t
    -> t

  val vertices : t -> (Point3.t * Point3.t * Point3.t)

  val normals : t -> (Vector3.t * Vector3.t * Vector3.t)

  val bsdf : t -> Bsdf.t
end

module Light : sig
  type t

  include Comparable.S with type t := t
  include Sexpable.S with type t := t
  include Stringable.S with type t := t

  val create :
    position : Point3.t
    -> power : Power3.t
    -> t

  val position : t -> Point3.t

  val power : t -> Power3.t
end

module Scene : sig
  type t

  include Sexpable.S with type t := t
  include Stringable.S with type t := t

  val create :
    triangles : Triangle.Set.t
    -> lights : Light.Set.t
    -> t

  val triangles : t -> Triangle.Set.t

  val lights : t -> Light.Set.t
end

module Camera : sig
  type t

  include Comparable.S with type t := t
  include Sexpable.S with type t := t
  include Stringable.S with type t := t

  val create :
    z_near : float
    -> z_far : float
    -> field_of_view_x : float
    -> t

  val z_near : t -> float

  val z_far : t -> float

  val field_of_view_x : t -> float
end
