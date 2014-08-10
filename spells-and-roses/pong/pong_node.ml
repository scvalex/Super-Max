open Core.Std

module Direction = struct
  type t = Up | Down with bin_io, sexp, compare
end
open Direction

module Xy = struct
  type t = {
    x : float;
    y : float;
  } with bin_io, sexp
end
open Xy

let font = "UbuntuMono-B.ttf";;

module Bounding_box = struct
  type t = {
    top_left     : Xy.t;
    bottom_right : Xy.t;
  } with sexp

  let create ~top_left ~width ~height =
    let bottom_right =
      let x = top_left.x +. width in
      let y = top_left.y +. height in
      { x; y; }
    in
    { top_left; bottom_right; }
  ;;

  let collide a b =
    not Float.(a.bottom_right.y < b.top_left.y
               || a.top_left.y > b.bottom_right.y
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

module Player_id = struct
  module T = struct
    type t = A | B with bin_io, sexp, compare
  end

  include T
  include Comparable.Make_binable(T)
  include Sexpable.To_stringable(T)

  let other_player = function
    | A -> B
    | B -> A
  ;;
end
open Player_id.T

module Paddle = struct
  type t = {
    width         : float;
    height        : float;
    pos           : Xy.t;
    dims          : Xy.t;
    move_disp     : float;
    player        : Player_id.t;
  } with bin_io, sexp

  let create ~width ~height player =
    let dims =
      let x = (Float.min width height) /. 20.0 in
      let y = height /. 5.0 in
      { x; y; }
    in
    let pos =
      let x =
        match player with
        | A -> 0.0
        | B -> width -. dims.x
      in
      let y = (height -. dims.x) /. 2.0 in
      { x; y; }
    in
    let move_disp = height /. 2.0 /. 24.0 in
    { width; height; pos; dims; move_disp; player; }
  ;;

  let move t dir =
    let y =
      match dir with
      | Up   -> t.pos.y -. t.move_disp
      | Down -> t.pos.y +. t.move_disp
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

  let to_drawing t =
    let open Drawing in
    translate ~x:t.pos.x ~y:t.pos.y
      (colour ~r:1.0 ~g:1.0 ~b:1.0
         (rectangle ~width:t.dims.x ~height:t.dims.y ~filled:true))
  ;;
end

module Ball = struct
  type t = {
    width     : float;
    height    : float;
    pos       : Xy.t;
    ball_dim  : float;
    move_disp : Xy.t;
  } with bin_io, sexp, fields

  let start_pos ~width ~height ~ball_dim =
    let x = (width -. ball_dim) /. 2.0 in
    let y = (height -. ball_dim) /. 2.0 in
    { x; y; }
  ;;

  let create ~width ~height =
    let ball_dim = Float.min width height /. 15.0 in
    let pos = start_pos ~width ~height ~ball_dim in
    let move_disp =
      let vel = width /. 60.0 /. 5.0 in
      { x = vel; y = vel; }
    in
    { width; height; pos; ball_dim; move_disp; }
  ;;

  let reset t =
    let pos =
      start_pos ~width:t.width ~height:t.height ~ball_dim:t.ball_dim
    in
    { t with pos; }
  ;;

  let bounding_box t =
    Bounding_box.create ~top_left:t.pos ~width:t.ball_dim ~height:t.ball_dim
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
      then { move_disp with x = 0.0 -. move_disp.x; }
      else move_disp
    in
    { t with pos; move_disp; }
  ;;

  let to_drawing t =
    let open Drawing in
    translate ~x:t.pos.x ~y:t.pos.y
      (colour ~r:1.0 ~g:1.0 ~b:1.0
         (rectangle ~width:t.ball_dim ~height:t.ball_dim ~filled:true))
  ;;
end

module Player = struct
  module Id = Player_id

  type t = {
    id     : Id.t;
    paddle : Paddle.t;
    score  : int;
    moving : Direction.t option;
  } with bin_io, sexp, fields

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

    let create ~source ~step event =
      { source; step; event; }
    ;;
  end

  type t = {
    width             : float;
    height            : float;
    players           : Player.t Player.Id.Map.t;
    score             : int Player.Id.Map.t;
    players_connected : Player.Id.Set.t;
    ball              : Ball.t;
  } with bin_io, sexp

  let with_players t ~f =
    let player_a = Map.find_exn t.players A in
    let player_b = Map.find_exn t.players B in
    let player_a = f player_a in
    let player_b = f player_b in
    let players =
      Player.Id.Map.of_alist_exn
        [ (A, player_a); (B, player_b); ]
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
      Player.Id.Map.of_alist_exn [mk_player A; mk_player B;]
    in
    let players_connected = Player.Id.Set.empty in
    let ball = Ball.create ~width ~height in
    let score =
      Player.Id.Map.of_alist_exn [ (A, 0); (B, 0); ]
    in
    { width; height; players; players_connected; ball; score; }
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
      let {x = ball_x; _;} = Ball.pos ball in
      let player_lost =
        if Float.(ball_x < 0.0)
        then Some A
        else if Float.(ball_x > t.width)
        then Some B
        else None
      in
      match player_lost with
      | None ->
        { t with ball; }
      | Some player ->
        let ball = Ball.reset ball in
        let score =
          Map.change t.score (Player_id.other_player player) (fun score ->
              Some (1 + Option.value_exn ~here:_here_ score))
        in
        { t with ball; score; }
    end else begin
      t
    end
  ;;

  let on_event t ev =
    let print_player_set_changed ev_str players_connected =
      Mlog.m "%s.  Players connected: %s"
        ev_str
        (String.concat ~sep:", "
           (List.map ~f:Player.Id.to_string
              (Set.to_list players_connected)));
    in
    match Event.event ev with
    | Pong_event.Player_join id ->
      let players_connected = Set.add t.players_connected id in
      print_player_set_changed "Join" players_connected;
      {t with players_connected; }
    | Pong_event.Player_leave id ->
      let players_connected = Set.remove t.players_connected id in
      print_player_set_changed "Leave" players_connected;
      {t with players_connected; }
    | Pong_event.Move (id, dir) ->
      with_player t ~id ~f:(fun player ->
          Player.move player dir)
  ;;

  let to_drawing t =
    let open Drawing in
    let score =
      many
        [ translate ~x:(t.width /. 2.0 -. t.width /. 10.0) ~y:10.0
            (text ~font ~size_pt:32 ~position:(`X `Right, `Y `Top)
               [sprintf "%+d" (Map.find_exn t.score A)])
        ; translate ~x:(t.width /. 2.0 +. t.width /. 10.0) ~y:10.0
            (text ~font ~size_pt:32 ~position:(`X `Left, `Y `Top)
               [sprintf "%+d" (Map.find_exn t.score B)])
        ]
    in
    many
      [ Ball.to_drawing t.ball
      ; Paddle.to_drawing (Player.paddle (Map.find_exn t.players A))
      ; Paddle.to_drawing (Player.paddle (Map.find_exn t.players B))
      ; score
      ]
  ;;

  let ball_bounding_box t =
    Ball.bounding_box t.ball
  ;;

  let paddles_bounding_boxes t =
    Player.Id.Map.of_alist_exn
      [ (A, Paddle.bounding_box (Player.paddle (Map.find_exn t.players A)))
      ; (B, Paddle.bounding_box (Player.paddle (Map.find_exn t.players B)))
      ]
  ;;
end

module Node = Node.Make(State)
module Event = State.Event

type t = Node.t

let create ~width ~height =
  Node.create ~step:0 ~history_length:60
    ~state:(State.create ~width ~height)
;;

let create_with_state state =
  Node.create ~step:0 ~history_length:60 ~state
;;

let state t =
  Node.state t
;;

let to_drawing t =
  State.to_drawing (Node.state t)
;;

let on_step t =
  Node.on_step t
;;

let on_event t ev =
  Node.add_event t ev
  |> Or_error.ok_exn
;;

let ball_bounding_box t =
  State.ball_bounding_box (Node.state t)
;;

let paddles_bounding_boxes t =
  State.paddles_bounding_boxes (Node.state t)
;;
