open Super_max.Std

type world

type state

val create_world : unit -> world

module Grass : sig
  val create : unit -> (state, world) Entity.t
end

module Cliff_s : sig
  val create : unit -> (state, world) Entity.t
end

val is_obstacle : (state, world) Entity.t -> bool
