open Core.Std

module T = struct
  type t =
    | Escape
    | Quit
    | Down | Up | Left | Right
  with sexp, variants, compare
end

include T
include Comparable.Make(T)
include Sexpable.To_stringable(T)

let of_sdl_key = function
  | `Down  -> Some Down
  | `Up    -> Some Up
  | `Left  -> Some Left
  | `Right -> Some Right
  | _      -> None
;;
