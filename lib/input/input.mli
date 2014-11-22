open Core.Std
open Sdl_lib.Std

module Snapshot : sig
  type t

  val pressed : t -> Key.t -> bool

  (** [mouse_position] is the cursor's location in window coordinates
      with origin at the top-left corner. *)
  val mouse_position : t -> (int * int)
end

val current_snapshot : unit -> Snapshot.t

val process_events : Sdl.window -> Snapshot.t
