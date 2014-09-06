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

  let scale t s =
    { x = t.x *. s; y = t.y *. s; z = t.z *. s; }
  ;;

  let magnitude t =
    sqrt (t.x *. t.x +. t.y *. t.y +. t.z *. t.z)
  ;;

  let direction t =
    scale t (1.0 /. magnitude t)
  ;;

  let (+) a b =
    { x = a.x +. b.x; y = a.y +. b.y; z = a.z +. b.z; }
  ;;

  let of_vector t = t;;
end

module Point2 = Vector2

module Point3 = struct
  include Vector3

  let add_vector = (+);;
end

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

  let of_vector v3 =
    { r = Vector3.x v3; g = Vector3.y v3; b = Vector3.z v3; }
  ;;

  let of_color t = t;;
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

  let create ~origin ~direction =
    let direction = Vector3.direction direction in
    { origin; direction; }
  ;;
end

module Bsdf = struct
  type t = unit with compare, sexp

  let test = ();;
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
