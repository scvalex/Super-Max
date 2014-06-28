open Core.Std

module Event = Pong_node.Event
open Pong_node.Pong_event
open Pong_node.Player.Id

type t = {
  node : Pong_node.t;
}

let steps_per_sec = 60.0;;

let create ~width ~height =
  let width = Float.of_int width in
  let height = Float.of_int height in
  let node = Pong_node.create ~width ~height in
  let node =
    let player = Node.Id.of_string "player" in
    let computer = Node.Id.of_string "computer" in
    let node =
      Pong_node.on_event node
        (Event.create ~source:player ~step:0 (Player_join A))
    in
    let node =
      Pong_node.on_event node
        (Event.create ~source:computer ~step:0 (Player_join B))
    in
    node
  in
  { node; }
;;

let to_drawing t =
  let open Drawing in
  many
    [ Pong_node.to_drawing t.node
    ]
;;

let on_step t =
  let node = Pong_node.step t.node in
  `Continue { t with node; }
;;

let on_event t ev =
  match ev with
  | Sdlevent.Quit _
  | Sdlevent.KeyUp {Sdlevent. keycode = Sdlkeycode.Q; _} ->
    `Quit
  | _ ->
    `Continue t
;;
