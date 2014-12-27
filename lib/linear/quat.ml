open Core.Std

include Types_intf.Quat

let create ~s x y z =
  { s; x; y; z; }
;;

let to_string t =
  sprintf "[%.2f, %.2f %.2f %.2f]" t.s t.x t.y t.z
;;

let scale t f =
  {
    s = t.s *. f;
    x = t.x *. f;
    y = t.y *. f;
    z = t.z *. f;
  }
;;

let conjugate t =
  {
    s = t.s;
    x = -. t.x;
    y = -. t.y;
    z = -. t.z;
  }
;;

let norm_sq t =
  t.s *. t.s +. t.x *. t.x +. t.y *. t.y +. t.z *. t.z
;;

let norm t =
  sqrt (norm_sq t)
;;

let inverse t =
  scale (conjugate t) (norm_sq t)
;;

let dot a b =
  a.s *. b.s +. a.x *. b.x +. a.y *. b.y +. a.z *. b.z
;;

module O = struct
  let ( + ) a b =
    {
      s = a.s +. b.s;
      x = a.x +. b.x;
      y = a.y +. b.y;
      z = a.z +. b.z;
    }
  ;;

  let ( - ) a b =
    {
      s = a.s -. b.s;
      x = a.x -. b.x;
      y = a.y -. b.y;
      z = a.z -. b.z;
    }
  ;;

  let ( * ) a b =
    {
      s = a.s *. b.s -. a.x *. b.x -. a.y *. b.y -. a.z *. b.z;
      x = a.s *. b.x +. b.s *. a.x +. a.y *. b.z +. b.y *. a.z;
      y = a.s *. b.y +. b.s *. a.y +. a.z *. b.x +. b.z *. a.x;
      z = a.s *. b.z +. b.s *. a.z +. a.x *. b.y +. b.x *. a.y;
    }
  ;;
end
