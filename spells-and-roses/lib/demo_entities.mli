type world

type common

val create_world : unit -> world

module Grass : sig
  val create : unit -> (common, world) Entity.t
end

module Cliff_s : sig
  val create : unit -> (common, world) Entity.t
end

val is_obstacle : (common, world) Entity.t -> bool
