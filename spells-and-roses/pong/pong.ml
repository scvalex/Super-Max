open Core.Std
open Async.Std
open Ocaml_plugin.Std

module Event = Pong_logic.Event
module Pong_event = Pong_logic.Pong_event
module Id = Pong_logic.Player.Id
module Dir = Pong_logic.Direction

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
      type t = Nothing.t with bin_io, sexp
    end

    module Snapshot = struct
      type t = Nothing.t with bin_io, sexp
    end

    type t = Nothing.t with bin_io, sexp
  end

  type t = {
    mutable logic  : Pong_logic.t;
    mutable player : Pong_player.t;
    playing_as     : Id.t;
    mutable step   : int;
  }

  let steps_per_second = 60.0;;

  let player_event pong_player step pong_event =
    Event.create ~source:(Pong_player.source pong_player) ~step pong_event
  ;;

  let create ~width ~height =
    let width = Float.of_int width in
    let height = Float.of_int height in
    let logic =
      Pong_logic.create ~width ~height
        ~history_rewrite_cutoff:(Float.iround_exn steps_per_second)
    in
    let playing_as =
      match Args.mode with
      | `Host         -> Id.A
      | `Connect_to _ -> Id.B
    in
    let player = Pong_player.create ~width ~height ~playing_as in
    let step = 0 in
    let logic =
      let computer = Logic_world.Source_id.of_string "computer" in
      let logic =
        Pong_logic.on_event logic
          (player_event player step (Pong_event.Player_join playing_as))
      in
      let logic =
        Pong_logic.on_event logic
          (Event.create ~source:computer ~step (Pong_event.Player_join Id.B))
      in
      logic
    in
    { logic; player; playing_as; step; }
  ;;

  let to_drawing t =
    let open Drawing in
    many
      [ Pong_logic.to_drawing t.logic
      ]
  ;;

  let on_step t ~engine:_ =
    let game_state =
      let paddles = Pong_logic.paddles_bounding_boxes t.logic in
      let ball = Pong_logic.ball_bounding_box t.logic in
      { Pong_player_intf.Game_state. paddles; ball; }
    in
    let (player, dir_a) = Pong_player.on_step t.player game_state in
    t.player <- player;
    Option.iter dir_a ~f:(fun dir_a ->
      t.logic <- Pong_logic.on_event t.logic
                   (player_event player t.step (Pong_event.Move (t.playing_as, dir_a)));
      t.logic <- Pong_logic.on_step t.logic);
    t.step <- t.step + 1;
  ;;

  let on_event t ~engine ev =
    match Pong_player.on_event t.player ev with
    | `Quit ->
      Engine.quit engine
    | `Continue player ->
      t.player <- player
  ;;

  let on_update_query _t ~engine:_ ~peer_id:_ query =
    Nothing.unreachable_code query
  ;;

  let on_update _t ~engine:_ ~peer_id:_ update =
    Nothing.unreachable_code update
  ;;
end
