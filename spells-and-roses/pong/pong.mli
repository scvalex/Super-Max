open Async.Std

val load_player : file : string -> (module Pong_player_intf.S) Deferred.t

module Make(Pong_player : Pong_player_intf.S) : sig
  type t

  include Game.S with type t := t
end
