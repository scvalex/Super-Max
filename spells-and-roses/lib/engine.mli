open Core.Std

(** The [Engine.t] is the game engine's interface for game
    callbacks. *)
type 'u t

val broadcast : 'u t -> 'u -> unit

val quit : _ t -> unit

module Internal : sig
  val create : unit -> 'u t

  val drain_updates : 'u t -> 'u Queue.t

  val quitting : _ t -> bool
end
