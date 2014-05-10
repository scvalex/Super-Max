open Core.Std
open Async.Std

val edit :
     file : String.t
  -> unit Deferred.t
