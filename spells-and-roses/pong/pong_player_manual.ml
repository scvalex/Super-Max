open Core.Std let _ = _squelch_unused_module_warning_
open Pong_node.Direction

module Input_event = struct
  module T = struct
    type t = [ `Up | `Down ] with sexp, compare
  end

  include T
  include Comparable.Make(T)
end

type t = {
  active_events : Input_event.Set.t;
  step          : int;
}

let create ~width:_ ~height:_ ~playing_as:_ =
  let active_events = Input_event.Set.empty in
  let step = 0 in
  { active_events; step; }
;;

let on_step t _game_state =
  let step = t.step + 1 in
  let t = { t with step; } in
  let direction =
    if Set.mem t.active_events `Up
    then Some Up
    else if Set.mem t.active_events `Down
    then Some Down
    else None
  in
  (t, direction)
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

let source _t =
  Node.Id.of_string (Unix.getlogin ())
;;
