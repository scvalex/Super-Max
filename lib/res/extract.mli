open Core.Std
open Async.Std

val extract_mesh :
  source : string
  -> geometry_id : string
  -> target : string
  -> unit Deferred.Or_error.t
