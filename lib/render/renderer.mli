open Core.Std
open Async.Std
open Res_lib.Std
open Sdl_lib.Std

module Ui : sig
  type 'a t
end

type t

val with_renderer : (t -> unit Deferred.t) -> unit Deferred.t

val on_ui_thread : t -> 'a Ui.t -> 'a Deferred.t

val with_sdl_window : t -> (Sdl.window -> 'a) -> 'a Ui.t

(** Just show some simple graphics. *)
val test : t -> unit Deferred.t

val render_mesh :
  t
  -> mesh : Res.Mesh.t
  -> program : Res.Program.t
  -> unit Ui.t
