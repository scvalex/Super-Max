open Core.Std
open Async.Std

val load_player : file : string -> (module Pong_player_intf.S) Deferred.t

module type Args = sig
  val mode : [`Host | `Connect_to of Host_and_port.t]
end

module Make(Pong_player : Pong_player_intf.S)(Args : Args) : sig
  include Game.S
end
