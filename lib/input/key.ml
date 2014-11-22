open Core.Std

module T = struct
  type t =
    | Escape
    | Quit
  with sexp, variants, compare
end

include T
include Comparable.Make(T)
include Sexpable.To_stringable(T)
