open Async.Std

val load_player : file : string -> (module Pong_player_intf.S) Deferred.t

module type Args = sig
  val mode : [`Host | `Connect_to of string]
end

module Make(Pong_player : Pong_player_intf.S)(Args : Args) : sig
  type t

  include Game.S with type t := t
end
