open Super_max.Std

type t

include World_intf.Zelda with type t := t
include Game.S with type t := t
