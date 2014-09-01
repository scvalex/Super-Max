open Core.Std

module Vector2 = struct
  module T = struct
    type t = {
      x : float;
      y : float;
    } with compare, sexp, fields
  end

  include T
  include Comparable.Make(T)
  include Sexpable.To_stringable(T)

  let create = Fields.create;;
end

module Vector3 = struct
  module T = struct
    type t = {
      x : float;
      y : float;
      z : float;
    } with compare, sexp, fields
  end

  include T
  include Comparable.Make(T)
  include Sexpable.To_stringable(T)

  let create = Fields.create;;

  let xyz t = (t.x, t.y, t.z);;
end

module Point2 = Vector2

module Point3 = Vector3

module Color3 = struct
  module T = struct
    type t = {
      r : float;
      g : float;
      b : float;
    } with compare, sexp, fields
  end

  include T
  include Comparable.Make(T)
  include Sexpable.To_stringable(T)

  let create = Fields.create;;

  let rgb t = (t.r, t.g, t.b);;
end

module Radiance3 = Color3

module Power3 = Color3

module Ray = struct
  module T = struct
    type t = {
      origin    : Point3.t;
      direction : Vector3.t;
    } with compare, sexp, fields
  end

  include T
  include Comparable.Make(T)
  include Sexpable.To_stringable(T)

  let create = Fields.create;;
end
