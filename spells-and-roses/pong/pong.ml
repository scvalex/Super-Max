open Core.Std let _ = _squelch_unused_module_warning_

type t = {
  node : Pong_node.t;
}

let steps_per_sec = 60.0;;

let create ~width:_ ~height:_ =
  let node = Pong_node.create () in
  ()
;;

let to_drawing _t =
  let open Drawing in
  many []
;;

let on_step t =
  `Continue t
;;

let on_event t ev =
  match ev with
  | Sdlevent.Quit _
  | Sdlevent.KeyUp {Sdlevent. keycode = Sdlkeycode.Q; _} ->
    `Quit
  | _ ->
    `Continue t
;;
