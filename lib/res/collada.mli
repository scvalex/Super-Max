open Core.Std
open Async.Std

val extract_mesh :
  source : string
  -> source_id : string
  -> (Float_array.t * Int_array.t) Deferred.Or_error.t
