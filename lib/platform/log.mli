open Core.Std

module Tag : sig
  type t =
    [ `Input
    | `Renderer
    | `Camera
    ] with sexp
end

val log : Tag.t -> ('a, unit, string, unit) format4 -> 'a
