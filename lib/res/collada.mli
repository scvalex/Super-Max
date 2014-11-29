open Core.Std
open Async.Std
open Linear_lib.Std

val extract_mesh :
  source : string
  -> source_id : string
  -> (Rarray.Float.t * Rarray.Int.t) Deferred.Or_error.t
