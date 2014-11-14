open Core.Std
open Async.Std

val extract_mesh :
  source : string
  -> source_id : string
  -> target_id : string
  -> unit Deferred.Or_error.t