open Core.Std
open Async.Std
open Ocaml_plugin.Std

module Event = Pong_node.Event
module Pong_event = Pong_node.Pong_event
module Id = Pong_node.Player.Id
module Dir = Pong_node.Direction

module Pong_player_compiler = Ocaml_compiler.Make(struct
  type t = (module Pong_player_intf.S)
  let t_repr = "Pong_player_intf.S";;
  let univ_constr = Pong_player_intf.univ_constr;;
  let univ_constr_repr = "Pong_player_intf.univ_constr";;
end)

let load_player ~file =
  Pong_player_compiler.load_ocaml_src_files [file]
  |! Deferred.Or_error.ok_exn
;;

module type Args = sig
  val mode : [`Host | `Connect_to of Host_and_port.t]
end

module Make(Pong_player : Pong_player_intf.S)(Args : Args) = struct
  module Update = struct
    module Query = struct
      type t = unit with bin_io, sexp
    end

    type t = unit with bin_io, sexp
  end

  type t = {
    node       : Pong_node.t;
    player     : Pong_player.t;
    playing_as : Id.t;
    step       : int;
  }

  let steps_per_second = 60.0;;

  let player_event pong_player step pong_event =
    Event.create ~source:(Pong_player.source pong_player) ~step pong_event
  ;;

  let create ~width ~height =
    let width = Float.of_int width in
    let height = Float.of_int height in
    let node =
      Pong_node.create ~width ~height
        ~history_rewrite_cutoff:(Float.iround_exn steps_per_second)
    in
    let playing_as =
      match Args.mode with
      | `Host         -> Id.A
      | `Connect_to _ -> Id.B
    in
    let player = Pong_player.create ~width ~height ~playing_as in
    let step = 0 in
    let node =
      let computer = Node.Id.of_string "computer" in
      let node =
        Pong_node.on_event node
          (player_event player step (Pong_event.Player_join playing_as))
      in
      let node =
        Pong_node.on_event node
          (Event.create ~source:computer ~step (Pong_event.Player_join Id.B))
      in
      node
    in
    { node; player; playing_as; step; }
  ;;

  let to_drawing t =
    let open Drawing in
    many
      [ Pong_node.to_drawing t.node
      ]
  ;;

  let on_step t =
    let game_state =
      let paddles = Pong_node.paddles_bounding_boxes t.node in
      let ball = Pong_node.ball_bounding_box t.node in
      { Pong_player_intf.Game_state. paddles; ball; }
    in
    let (player, dir_a) = Pong_player.on_step t.player game_state in
    let node =
      Option.value_map dir_a ~default:t.node
        ~f:(fun dir_a ->
            Pong_node.on_event t.node
              (player_event player t.step (Pong_event.Move (t.playing_as, dir_a))))
    in
    let node = Pong_node.on_step node in
    let step = t.step + 1 in
    `Continue { t with node; player; step; }
  ;;

  let on_event t ev =
    match Pong_player.on_event t.player ev with
    | `Quit ->
      `Quit
    | `Continue player ->
      `Continue { t with player; }
  ;;

  let on_update_query t _query =
    (t, `Reject)
  ;;

  let on_update t _update =
    t
  ;;
end
