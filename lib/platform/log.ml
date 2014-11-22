open Core.Std

module Log = Async.Std.Log

module Tag = struct
  module T = struct
    type t =
      [ `Input
      ] with sexp, compare
  end

  include T
  include Comparable.Make(T)
  include Sexpable.To_stringable(T)
end

module Debug_tags = struct
  type t = Tag.Set.t with sexp
end

let debug_tags =
  match Core.Std.Sys.getenv "DEBUG" with
  | None      -> Tag.Set.empty
  | Some tags -> Debug_tags.t_of_sexp (Sexp.of_string ("(" ^ tags ^ ")"))
;;

let log tag fmt =
  ksprintf (fun str ->
    if Set.mem debug_tags tag then
      Log.Global.info "%s" str) fmt
;;
