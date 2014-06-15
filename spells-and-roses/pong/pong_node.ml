open Core.Std

module Direction = struct
  type t = Up | Down with sexp, compare
end

type xy = {
  x : float;
  y : float;
}

module Bounding_box = struct
  type t = {
    top_left     : xy;
    bottom_right : xy;
  }

  let create ~top_left ~width ~height =
    let bottom_right =
      let x = top_left.x +. width in
      let y = top_left.y +. height in
      { x; y; }
    in
    { top_left; bottom_right; }
  ;;

  let collide a b =
    not Float.(a.bottom_right.y > b.top_left.y
               || a.top_left.y < b.bottom_right.y
               || a.bottom_right.x < b.top_left.x
               || a.top_left.x > b.bottom_right.x)
  ;;
end

let snapback ~lower ~upper x =
  if Float.(x < lower)
  then (lower, `Lower)
  else if Float.(x > upper)
  then (upper, `Upper)
  else (x, `No_snapback)
;;

type player_id = [`A | `B] with sexp, compare

module Paddle = struct
  type t = {
    width         : float;
    height        : float;
    pos           : xy;
    dims          : xy;
    move_disp     : float;
    player        : player_id;
  }

  let create ~width ~height player =
    let dims =
      let x = (Float.min width height) /. 20.0 in
      let y = height /. 5.0 in
      { x; y; }
    in
    let pos =
      let x =
        match player with
        | `A -> 0.0
        | `B -> width -. dims.x
      in
      let y = (height -. dims.x) /. 2.0 in
      { x; y; }
    in
    let move_disp = height /. 2.0 /. 6.0 in
    { width; height; pos; dims; move_disp; player; }
  ;;

  let move t dir =
    let y =
      match dir with
      | Direction.Up   -> t.pos.y -. t.move_disp
      | Direction.Down -> t.pos.y -. t.move_disp
    in
    let (y, _) =
      snapback y ~lower:0.0 ~upper:(t.height -. t.dims.y)
    in
    let pos = { t.pos with y; } in
    { t with pos; }
  ;;

  let bounding_box t =
    Bounding_box.create ~top_left:t.pos ~width:t.dims.x ~height:t.dims.y
  ;;
end

module Ball = struct
  type t = {
    width     : float;
    height    : float;
    pos       : xy;
    ball_dim  : float;
    move_disp : xy;
  }

  let start_pos ~width ~height ~ball_dim =
    let x = (width -. ball_dim) /. 2.0 in
    let y = (height -. ball_dim) /. 2.0 in
    { x; y; }
  ;;

  let create ~width ~height =
    let ball_dim = Float.min width height /. 10.0 in
    let pos = start_pos ~width ~height ~ball_dim in
    let move_disp =
      let vel = width /. 60.0 /. 5.0 in
      { x = vel; y = vel; }
    in
    { width; height; pos; ball_dim; move_disp; }
  ;;

  let _reset t =
    let pos =
      start_pos ~width:t.width ~height:t.height ~ball_dim:t.ball_dim
    in
    { t with pos; }
  ;;

  let step t ~a_box ~b_box =
    let x = t.pos.x +. t.move_disp.x in
    let (y, snap) =
      snapback (t.pos.y +. t.move_disp.y)
        ~lower:0.0 ~upper:(t.height -. t.ball_dim)
    in
    let pos = { x; y; } in
    let box =
      Bounding_box.create ~top_left:pos ~width:t.ball_dim ~height:t.ball_dim
    in
    (* If we hit the upper or lower screen edge, reflect vertically. *)
    let move_disp =
      match snap with
      | `No_snapback ->
        t.move_disp
      | `Lower | `Upper ->
        { t.move_disp with y = 0.0 -. t.move_disp.y; }
    in
    (* If we hit either of the paddles, reflect horizontally. *)
    let collide_a = Bounding_box.collide box a_box in
    let collide_b = Bounding_box.collide box b_box in
    let move_disp =
      if collide_a || collide_b
      then { move_disp with x = 0.0 -. move_disp.y; }
      else move_disp
    in
    { t with pos; move_disp; }
  ;;
end

module Player = struct
  module Id = struct
    module T = struct
      type t = player_id with sexp, compare
    end

    include T
    include Comparable.Make(T)
  end

  type t = {
    id     : Id.t;
    paddle : Paddle.t;
    score  : int;
    moving : Direction.t option;
  } with fields

  let create id ~height ~width =
    let paddle = Paddle.create ~width ~height id in
    let score = 0 in
    let moving = None in
    { id; paddle; score; moving; }
  ;;

  let step t =
    let paddle =
      match t.moving with
      | None     -> t.paddle
      | Some dir -> Paddle.move t.paddle dir
    in
    { t with moving = None; paddle; }
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
    ball              : Ball.t;
  }

  let with_players t ~f =
    let player_a = Map.find_exn t.players `A in
    let player_b = Map.find_exn t.players `B in
    let player_a = f player_a in
    let player_b = f player_b in
    let players =
      Player.Id.Map.of_alist_exn
        [ (`A, player_a); (`B, player_b); ]
    in
    ({ t with players; }, `A player_a, `B player_b)
  ;;

  let with_player ~id t ~f =
    let player = Map.find_exn t.players id in
    let player = f player in
    let players = Map.add t.players ~key:id ~data:player in
    { t with players; }
  ;;

  let create ~width ~height =
    let mk_player id =
      (id, Player.create id ~width ~height)
    in
    let players =
      Player.Id.Map.of_alist_exn [mk_player `A; mk_player `B;]
    in
    let players_connected = Player.Id.Set.empty in
    let ball = Ball.create ~width ~height in
    { players; players_connected; ball; }
  ;;

  let on_step t =
    if Int.(Player.Id.Set.length t.players_connected = 2)
    then begin
      let (t, `A player_a, `B player_b) =
        with_players t ~f:Player.step
      in
      let ball =
        Ball.step t.ball
          ~a_box:(Paddle.bounding_box (Player.paddle player_a))
          ~b_box:(Paddle.bounding_box (Player.paddle player_b))
      in
      { t with ball; }
    end else begin
      t
    end
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

let create ~width ~height =
  Node.create ~step:0 ~history_length:60
    ~state:(State.create ~width ~height)
;;
