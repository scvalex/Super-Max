open Core.Std let _ = _squelch_unused_module_warning_
(* open Sdl_lib.Std *)

module Snapshot = struct
  type t = {
    pressed : Key.Set.t;
  }

  let create () =
    let pressed = Key.Set.empty in
    { pressed; }
  ;;

  let pressed t key =
    Set.mem t.pressed key
  ;;
end

type t = {
  snapshot : Snapshot.t;
}

let t =
  let snapshot = Snapshot.create () in
  { snapshot; }
;;

let current_snapshot () =
  t.snapshot
;;

let process_events _window =
  failwith "not implemented";
  (* current_snapshot () *)
;;
