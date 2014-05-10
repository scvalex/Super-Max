open Super_max.Std

type world

val create_world : unit -> world

module Grass : sig
  val create : unit -> world Entity.t
end
