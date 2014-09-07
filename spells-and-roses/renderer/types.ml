open Core.Std

let m_pi = 3.14159;;

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

  let (-) a b =
    { x = a.x -. b.x; y = a.y -. b.y; z = a.z -. b.z; }
  ;;

  let dot a b =
    a.x *. b.x +. a.y *. b.y +. a.z *. b.z
  ;;

  let cross a b =
    { x = a.y *. b.z -. a.z *. b.y;
      y = a.z *. b.x -. a.x *. b.z;
      z = a.x *. b.y -. a.y *. b.x;
    }
  ;;

  let of_vector t = t;;

  let to_vector t = t;;
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

  let scale t s =
    { r = t.r *. s; g = t.g *. s; b = t.b *. s; }
  ;;

  let (+) a b =
    { r = a.r +. b.r; g = a.g +. b.g; b = a.b +. b.b; }
  ;;

  let ( * ) a b =
    { r = a.r *. b.r; g = a.g *. b.g; b = a.b *. b.b; }
  ;;

  let of_color t = t;;

  let to_radiance t = t;;
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
  module Lambertian_reflectance = struct
    type t = {
      k_l : Radiance3.t;
    } with fields, compare, sexp

    let evaluate_finite_scattering_density t ~w_i:_ ~w_o:_ =
      Radiance3.scale t.k_l (1.0 /. m_pi)
    ;;

    let create = Fields.create;;
  end

  module Blinn_phong = struct
    type t = {
      k_l : Radiance3.t;
      k_g : Radiance3.t;
      s   : float;
      n   : Vector3.t;          (* CR scvalex: Where do I get n from? *)
    } with fields, compare, sexp

    let evaluate_finite_scattering_density t ~w_i ~w_o =
      let w_h = Vector3.(direction (w_i + w_o)) in
      Radiance3.(t.k_l
                 + scale t.k_g ((t.s +. 8.0)
                                *. Float.(max 0.0 (Vector3.dot w_h t.n))
                                   ** t.s /. m_pi))
    ;;

    let create = Fields.create;;
  end

  module T = struct
    type t =
      | Lambertian_reflectance of Lambertian_reflectance.t
      | Blinn_phong of Blinn_phong.t
    with compare, sexp
  end

  include T
  include Comparable.Make(T)
  include Sexpable.To_stringable(T)

  let evaluate_finite_scattering_density t ~w_i ~w_o =
    match t with
    | Lambertian_reflectance t ->
      Lambertian_reflectance.evaluate_finite_scattering_density t ~w_i ~w_o
    | Blinn_phong t ->
      Blinn_phong.evaluate_finite_scattering_density t ~w_i ~w_o
  ;;

  let lambertian_reflectance ~k_l =
    Lambertian_reflectance (Lambertian_reflectance.create ~k_l)
  ;;

  let blinn_phong ~k_l ~k_g ~s ~n =
    Blinn_phong (Blinn_phong.create ~k_l ~k_g ~s ~n)
  ;;
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
