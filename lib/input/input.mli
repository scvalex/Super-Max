open Core.Std
open Sdl_lib.Std

module Snapshot : sig
  type t
end

val current_snapshot : unit -> Snapshot.t

val handle_events : Sdl.window -> Snapshot.t
