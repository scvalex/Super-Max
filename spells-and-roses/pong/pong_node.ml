open Core.Std

module Direction = struct
  type t = Up | Down with sexp, compare
end

module Paddle = struct
  type t = unit

  let create () =
    ()
  ;;
end

module Player = struct
  module Id = struct
    module T = struct
      type t = A | B with sexp, compare
    end

    include T
    include Comparable.Make(T)
  end

  type t = {
    id     : Id.t;
    paddle : Paddle.t;
    score  : int;
    moving : Direction.t option;
  }

  let create id =
    let paddle = Paddle.create () in
    let score = 0 in
    let moving = None in
    { id; paddle; score; moving; }
  ;;

  let step t =
    { t with moving = None; }
  ;;

  let move t dir =
    { t with moving = Some dir; }
  ;;
end

module Pong_event = struct
  type t =
    | Player_join of Player.Id.t
    | Player_leave of Player.Id.t
    | Move of (Player.Id.t * Direction.t)
  with sexp, compare
end

module State = struct
  module Event = struct
    module T = struct
      type t = {
        source : Node.Id.t;
        step   : int;
        event  : Pong_event.t;
      } with fields, sexp, compare
    end

    include T
    include Comparable.Make(T)
  end

  type t = {
    players           : Player.t Player.Id.Map.t;
    players_connected : Player.Id.Set.t;
  }

  let with_players ~a ~b t =
    let player_a = Map.find_exn t.players Player.Id.A in
    let player_b = Map.find_exn t.players Player.Id.B in
    let player_a = a player_a in
    let player_b = b player_b in
    let players =
      Player.Id.Map.of_alist_exn
        [ (Player.Id.A, player_a)
        ; (Player.Id.B, player_b)
        ]
    in
    { t with players; }
  ;;

  let with_player ~id t ~f =
    let player = Map.find_exn t.players id in
    let player = f player in
    let players = Map.add t.players ~key:id ~data:player in
    { t with players; }
  ;;

  let create () =
    let players =
      Player.Id.Map.of_alist_exn
        [ (Player.Id.A, Player.create Player.Id.A)
        ; (Player.Id.B, Player.create Player.Id.B)
        ]
    in
    let players_connected = Player.Id.Set.empty in
    { players; players_connected; }
  ;;

  let on_step t =
    if Int.(Player.Id.Set.length t.players_connected = 2)
    then
      with_players t
        ~a:Player.step
        ~b:Player.step
    else
      t
  ;;

  let on_event t ev =
    match Event.event ev with
    | Pong_event.Player_join id ->
      let players_connected = Set.add t.players_connected id in
      {t with players_connected; }
    | Pong_event.Player_leave id ->
      let players_connected = Set.remove t.players_connected id in
      {t with players_connected; }
    | Pong_event.Move (id, dir) ->
      with_player t ~id ~f:(fun player ->
          Player.move player dir)
  ;;
end

module Node = Node.Make(State)

type t = Node.t

let create () =
  Node.create ~step:0 ~state:(State.create ()) ~history_length:60
;;
