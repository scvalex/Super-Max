open Core.Std let _ = _squelch_unused_module_warning_
(* open Sdl_lib.Std *)

module Snapshot = struct
  type t = unit

  let create () =
    ()
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

let handle_events _window =
  current_snapshot ()
;;
