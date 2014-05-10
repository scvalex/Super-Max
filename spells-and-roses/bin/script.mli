open Async.Std

val run :
     file : string
  -> args : string list
  -> unit Deferred.t

val check_load : file : string -> unit Deferred.t

