open Core.Std
open Async.Std
open Sdl_lib.Std

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

  let add_sdl_event t event =
    match event with
    | `Quit ->
      { t with pressed = Set.add t.pressed Key.quit; }
    | `Unknown str ->
      Log.Global.info "Unknown SDL event: %s" str;
      t
  ;;
end

type t = {
  mutable snapshot : Snapshot.t;
}

let t =
  let snapshot = Snapshot.create () in
  { snapshot; }
;;

let current_snapshot () =
  t.snapshot
;;

let process_events window =
  (* We take the window as a parameter to make sure we're on the Ui thread. *)
  let _ = window in
  let rec loop snapshot =
    match Sdl.poll_event () with
    | None       -> snapshot
    | Some event -> loop (Snapshot.add_sdl_event snapshot event)
  in
  t.snapshot <- loop t.snapshot;
  current_snapshot ()
;;
