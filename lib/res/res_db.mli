open Core.Std
open Async.Std

val add_search_path :
  dir : string
  -> unit

val load :
  id : Res.Id.t
  -> cache_until : [`Don't_cache | `End_of_days]
  -> Res.t Deferred.Or_error.t
