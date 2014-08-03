open Core.Std

module Event = Pong_node.Event
module Pong_event = Pong_node.Pong_event
module Id = Pong_node.Player.Id
module Dir = Pong_node.Direction

module Input_event = struct
  module T = struct
    type t = [ `Up | `Down ] with sexp, compare
  end

  include T
  include Comparable.Make(T)
end

type t = {
  node          : Pong_node.t;
  (* CR scvalex: Move event processing to an ocaml plugin file. *)
  active_events : Input_event.Set.t;
  step          : int;
}

let steps_per_sec = 60.0;;

let player = Node.Id.of_string "player";;

let create ~width ~height =
  let width = Float.of_int width in
  let height = Float.of_int height in
  let node = Pong_node.create ~width ~height in
  (* CR scvalex: Crutch. *)
  let step = 0 in
  let node =
    let computer = Node.Id.of_string "computer" in
    let node =
      Pong_node.on_event node
        (Event.create ~source:player ~step (Pong_event.Player_join Id.A))
    in
    let node =
      Pong_node.on_event node
        (Event.create ~source:computer ~step (Pong_event.Player_join Id.B))
    in
    node
  in
  let active_events = Input_event.Set.empty in
  { node; active_events; step; }
;;

let to_drawing t =
  let open Drawing in
  many
    [ Pong_node.to_drawing t.node
    ]
;;

let on_step t =
  let node =
    Set.fold_right t.active_events ~init:t.node ~f:(fun ev node ->
        match ev with
        | `Up ->
          Pong_node.on_event node
            (Event.create ~source:player ~step:t.step
               (Pong_event.Move (Id.A, Dir.Up)))
        | `Down ->
          Pong_node.on_event node
            (Event.create ~source:player ~step:t.step
               (Pong_event.Move (Id.A, Dir.Down))))
  in
  let node = Pong_node.on_step node in
  let step = t.step + 1 in
  `Continue { t with node; step; }
;;

let generic_handle_key_event t ~key ~event ev =
  match ev with
  | Sdlevent.KeyDown {Sdlevent. keycode; _} when key = keycode ->
    Some {t with active_events = Set.add t.active_events event; }
  | Sdlevent.KeyUp {Sdlevent. keycode; _} when key = keycode ->
    Some {t with active_events = Set.remove t.active_events event; }
  | _ ->
    None
;;

let on_event t ev =
  let keys_events =
    [ (Sdlkeycode.Down, `Down)
    ; (Sdlkeycode.Up, `Up)
    ]
  in
  let handled_t =
    List.fold_left keys_events ~init:None ~f:(fun acc_t (key, event) ->
        match acc_t with
        | Some t -> Some t
        | None   -> generic_handle_key_event t ~key ~event ev)
  in
  match handled_t with
  | Some t ->
    `Continue t
  | None ->
    match ev with
    | Sdlevent.Quit _
    | Sdlevent.KeyUp {Sdlevent. keycode = Sdlkeycode.Q; _} ->
      `Quit
    | _ ->
      `Continue t
;;
