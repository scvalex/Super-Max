open Core.Std
open Async.Std

type t

val with_renderer : (t -> unit Deferred.t) -> unit Deferred.t

(** Run any Gl or Sdl calls inside [on_ui_thread]. *)
val on_ui_thread : t -> (unit -> unit) -> unit Deferred.t

(** Just show some simple graphics. *)
val test : t -> unit Deferred.t
