open Async.Std

val edit :
     file : string
  -> data_dir : string
  -> unit Deferred.t
