open Core.Std

module Tag : sig
  type t =
    [ `Input
    ] with sexp
end

val log : Tag.t -> ('a, unit, string, unit) format4 -> 'a
