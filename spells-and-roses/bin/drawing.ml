open Core.Std

type xy = {
  x : float;
  y : float;
} with sexp

type width_height = {
  width  : float;
  height : float;
} with sexp

type rgba = {
  red   : float;
  green : float;
  blue  : float;
  alpha : float;
} with sexp

let rgb_a_of_colour rgba =
  let rgb =
    (Float.iround_exn (rgba.red *. 255.0),
     Float.iround_exn (rgba.green *. 255.0),
     Float.iround_exn (rgba.blue *. 255.0))
  in
  let a = Float.iround_exn (rgba.alpha *. 255.0) in
  (rgb, a)
;;

module Trans : sig
  type t

  include Sexpable.S with type t := t

  val id : t
  val scale : t -> xy -> t
  val translate : t -> xy -> t
  val apply : t -> xy -> xy
end = struct
  type t = Mat.mat3 with sexp

  let id = Mat.id ();;

  let scale t {x; y} =
    Mat.(t * scale ~x ~y)
  ;;

  let translate t {x; y} =
    Mat.(t * translate ~x ~y)
  ;;

  let apply t {x; y} =
    let (x, y) = Mat.(xy_of_vec3 (t *| vec3_of_xy ~x ~y)) in
    {x; y}
  ;;
end

type t =
  | Empty
  | Translate of (xy * t)
  | Scale of (xy * t)
  | Rectangle of width_height
  | Colour of (rgba * t)
  | Many of t list
with sexp

let empty = Empty;;

let translate ~x ~y t =
  Translate ({x; y;}, t)
;;

let scale ~x ~y t =
  if Float.(x < 0.0 || y < 0.0) then
    failwithf "Cannot scale by negative amounts: (%f, %f)" x y ();
  Scale ({x; y;}, t)
;;

let rectangle ~width ~height =
  if Float.(width < 0.0 || height < 0.0) then
    failwithf "Rectangle dimensions must be positive: (%f, %f)"
      width height ();
  Rectangle {width; height;}
;;

let colour ~r:red ~g:green ~b:blue ?a:(alpha = 1.0) t =
  if Float.(red < 0.0 || 1.0 < red
            || green < 0.0 || 1.0 < green
            || blue < 0.0 || 1.0 < blue
            || alpha < 0.0 || 1.0 < alpha)
  then
    failwithf "Invalid colour: (%f, %f, %f, %f)"
      red green blue alpha ();
  Colour ({red; green; blue; alpha;}, t)
;;

let many ts =
  Many ts
;;

let centered_normalized_scene ~width ~height t =
  let width = Float.of_int width in
  let height = Float.of_int height in
  let dim = Float.min width height in
  translate
    ~x:((width -. dim) /. 2.0)
    ~y:((height -. dim) /. 2.0)
    (scale ~x:dim ~y:dim t)
;;

let render t ~renderer =
  let rec loop trans colour = function
    | Empty ->
      ()
    | Translate (xy, t) ->
      let trans = Trans.translate trans xy in
      loop trans colour t
    | Scale (xy, t) ->
      let trans = Trans.scale trans xy in
      loop trans colour t
    | Rectangle {width; height} ->
      let xy0 = Trans.apply trans {x = 0.0; y = 0.0;} in
      let xy1 = Trans.apply trans {x = width; y = height;} in
      let (rgb, a) = rgb_a_of_colour colour in
      Sdlrender.set_draw_color renderer ~rgb ~a;
      Sdlrender.fill_rect renderer
        (Sdlrect.make4
           ~x:(Float.iround_exn xy0.x)
           ~y:(Float.iround_exn xy0.y)
           ~w:(Float.iround_exn (xy1.x -. xy0.x))
           ~h:(Float.iround_exn (xy1.y -. xy0.y)))
    | Colour (colour, t) ->
      loop trans colour t
    | Many ts ->
      List.iter ts ~f:(loop trans colour)
  in
  Sdlrender.set_draw_color renderer ~rgb:(0, 0, 0) ~a:255;
  Sdlrender.clear renderer;
  let white =
    {red = 1.0; green = 1.0; blue = 1.0; alpha = 1.0; }
  in
  loop Trans.id white t;
  Sdlrender.render_present renderer
;;

module Example = struct
  let rectangles ~width ~height =
    let half_square = rectangle ~width:0.48 ~height:0.48 in
    let rectangles =
      many
        [ translate ~x:0.01 ~y:0.01
            (colour ~r:1.0 ~g:0.0 ~b:0.0
               half_square)
        ; translate ~x:0.51 ~y:0.01
            (colour ~r:0.7 ~g:0.0 ~b:0.7
               half_square)
        ; translate ~x:0.01 ~y:0.51
            (colour ~r:0.7 ~g:0.0 ~b:0.7
               half_square)
        ; translate ~x:0.51 ~y:0.51
            (colour ~r:0.0 ~g:0.0 ~b:1.0
               half_square)
        ; translate ~x:0.4 ~y:0.1
            (colour ~r:0.0 ~g:0.5 ~b:0.0
               (rectangle ~width:0.59 ~height:0.3))
        ]
    in
    centered_normalized_scene rectangles ~width ~height
  ;;
end