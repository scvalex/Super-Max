open Core.Std

type common

val sprite_width : int

val sprite_height : int

module Grass : sig
  val create : unit -> (common, Sdlevent.t) Entity.t
end

module Cliff_s : sig
  val create : unit -> (common, Sdlevent.t) Entity.t
end

val is_obstacle : (common, Sdlevent.t) Entity.t -> bool

val entity_creators : (unit -> (common, Sdlevent.t) Entity.t) String.Map.t
