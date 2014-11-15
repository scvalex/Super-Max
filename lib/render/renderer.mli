open Core.Std
open Async.Std

type t

val with_renderer : (t -> unit Deferred.t) -> unit Deferred.t

val test : t -> unit Deferred.t
