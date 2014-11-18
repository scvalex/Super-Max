open Core.Std
open Async.Std

val add_pack :
  dir : string
  -> unit

val load :
  id : Res_id.t
  -> cache_until : [`Don't_cache | `End_of_days]
  -> Res.t Deferred.Or_error.t
