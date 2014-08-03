open Core.Std let _ = _squelch_unused_module_warning_
open Ocaml_plugin.Std

module Bounding_box = Pong_node.Bounding_box
module Id = Pong_node.Player.Id
module Direction = Pong_node.Direction

module Game_state = struct
  type t = {
    paddles : Bounding_box.t Id.Map.t;
    ball    : Bounding_box.t;
  } with sexp
end

module type S = sig
  type t

  val create :
    width : float
    -> height : float
    -> playing_as : Id.t
    -> t

  val on_step : t -> Game_state.t -> (t * Direction.t option)

  val on_event : t -> Sdlevent.t -> [`Continue of t | `Quit]

  val source : t -> Node.Id.t
end

let univ_constr : (module S) Ocaml_dynloader.Univ_constr.t =
  Ocaml_dynloader.Univ_constr.create ()
;;
