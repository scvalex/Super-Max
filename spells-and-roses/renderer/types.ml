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

module Bsdf = struct
  type t = unit with compare, sexp
end

module Triangle = struct
  module T = struct
    type t = {
      vertices : (Point3.t * Point3.t * Point3.t);
      normals  : (Vector3.t * Vector3.t * Vector3.t);
      bsdf     : Bsdf.t;
    } with compare, sexp, fields
  end

  include T
  include Comparable.Make(T)
  include Sexpable.To_stringable(T)

  let create = Fields.create;;
end

module Light = struct
  module T = struct
    type t = {
      position : Point3.t;
      power    : Power3.t
    } with compare, sexp, fields
  end

  include T
  include Comparable.Make(T)
  include Sexpable.To_stringable(T)

  let create = Fields.create;;
end

module Scene = struct
  module T = struct
    type t = {
      triangles : Triangle.Set.t;
      lights    : Light.Set.t;
    } with sexp, fields
  end

  include T
  include Sexpable.To_stringable(T)

  let create = Fields.create;;
end

module Camera = struct
  module T = struct
    type t = {
      z_near          : float;
      z_far           : float;
      field_of_view_x : float;
    } with compare, sexp, fields
  end

  include T
  include Comparable.Make(T)
  include Sexpable.To_stringable(T)

  let create = Fields.create;;
end
