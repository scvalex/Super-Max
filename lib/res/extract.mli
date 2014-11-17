open Core.Std
open Async.Std

val extract_mesh :
  source : string
  -> source_id : string
  -> target_id : string
  -> unit Deferred.Or_error.t

val extract_program :
  vertex : string
  -> fragment : string
  -> target_id : string
  -> unit Deferred.Or_error.t
