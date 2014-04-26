open Core.Std

type xy = {
  x : float;
  y : float;
} with sexp

type width_height = {
  width  : float;
  height : float;
} with sexp

type t =
  | Empty
  | Translate of (xy * t)
  | Scale of (xy * t)
  | Rectangle of width_height
with sexp

let empty = Empty;;

let translate ~x ~y t =
  Translate ({x; y;}, t)
;;

let scale ~x ~y t =
  Scale ({x; y;}, t)
;;

let rectangle ~width ~height =
  Rectangle {width; height;}
;;
