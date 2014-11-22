open Core.Std
open Platform_lib.Std
open Sdl_lib.Std

let log = log `Input;;

module Snapshot = struct
  type t = {
    pressed        : Key.Set.t;
    mouse_position : (int * int);
  } with fields

  let create () =
    let pressed = Key.Set.empty in
    let mouse_position = (0, 0) in
    { pressed; mouse_position; }
  ;;

  let pressed t key =
    Set.mem t.pressed key
  ;;

  let add_sdl_event t event =
    match event with
    | `Quit ->
      { t with pressed = Set.add t.pressed Key.quit; }
    | `Unknown str ->
      log "Unknown SDL event: %s" str;
      t
    | `Key (dir, key) -> begin
        match Key.of_sdl_key key with
        | None ->
          log "Unknown SDL key: %s" (Sexp.to_string_mach (Sdl.sexp_of_event event));
          t
        | Some key ->
          match dir with
          | `Down -> { t with pressed = Set.add t.pressed key; }
          | `Up   -> { t with pressed = Set.remove t.pressed key; }
      end
    | `Mouse_move (x, y) ->
      { t with mouse_position = (x, y); }
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
