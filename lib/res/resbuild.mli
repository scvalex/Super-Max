open Core.Std
open Async.Std

type t with sexp

val load :
  dir : string
  -> t Deferred.Or_error.t
