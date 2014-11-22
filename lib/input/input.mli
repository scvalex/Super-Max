open Core.Std
open Sdl_lib.Std

module Snapshot : sig
  type t

  val pressed : t -> Key.t -> bool
end

val current_snapshot : unit -> Snapshot.t

val process_events : Sdl.window -> Snapshot.t
