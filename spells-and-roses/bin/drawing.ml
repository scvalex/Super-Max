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
  Colour ({red; green; blue; alpha;}, t)
;;

let many ts =
  Many ts
;;

module Example = struct
  let rectangles =
    let half_square = rectangle ~width:0.48 ~height:0.48 in
    many
      [ translate ~x:0.01 ~y:0.01
          (colour ~r:1.0 ~g:0.0 ~b:0.0
             half_square)
      ; translate ~x:0.51 ~y:0.51
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
             (rectangle ~width:0.6 ~height:0.3))
    ]
  ;;
end
