open Core.Std
open Async.Std

val compile :
  dir : string
  -> unit Deferred.Or_error.t
