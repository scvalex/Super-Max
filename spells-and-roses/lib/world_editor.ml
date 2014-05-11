open Core.Std
open Async.Std
open Ocaml_plugin.Std

module World_plugin = Ocaml_compiler.Make(struct
  type t = (module World.S)
  let t_repr = "World.S";;
  let univ_constr = World.univ_constr;;
  let univ_constr_repr = "World.univ_constr";;
end)

let load ~file =
  World_plugin.load_ocaml_src_files [file]
  |! Deferred.Or_error.ok_exn
;;

module Event = struct
  module T = struct
    type t =
      [ `Up | `Down | `Left | `Right
      ] with sexp, compare
  end
  include T
  include Comparable.Make(T)
end

type state = {
  camera_x  : float;
  camera_y  : float;
  panning_x : float;
  panning_y : float;
  events    : Event.Set.t;
}

let create () =
  let camera_x = 0.0 in
  let camera_y = 0.0 in
  let panning_x = 0.0 in
  let panning_y = 0.0 in
  let events = Event.Set.empty in
  { camera_x; camera_y; panning_x; panning_y; events; }
;;

let on_event t ev =
  match ev with
  | Sdlevent.Quit _
  | Sdlevent.KeyUp {Sdlevent. keycode = Sdlkeycode.Q; _} ->
    `Quit
  | Sdlevent.KeyDown {Sdlevent. keycode = Sdlkeycode.Down; _} ->
    `Continue {t with events = Set.add t.events `Down; }
  | Sdlevent.KeyUp {Sdlevent. keycode = Sdlkeycode.Down; _} ->
    `Continue {t with events = Set.remove t.events `Down; }
  | Sdlevent.KeyDown {Sdlevent. keycode = Sdlkeycode.Up; _} ->
    `Continue {t with events = Set.add t.events `Up; }
  | Sdlevent.KeyUp {Sdlevent. keycode = Sdlkeycode.Up; _} ->
    `Continue {t with events = Set.remove t.events `Up; }
  | Sdlevent.KeyDown {Sdlevent. keycode = Sdlkeycode.Left; _} ->
    `Continue {t with events = Set.add t.events `Left; }
  | Sdlevent.KeyUp {Sdlevent. keycode = Sdlkeycode.Left; _} ->
    `Continue {t with events = Set.remove t.events `Left; }
  | Sdlevent.KeyDown {Sdlevent. keycode = Sdlkeycode.Right; _} ->
    `Continue {t with events = Set.add t.events `Right; }
  | Sdlevent.KeyUp {Sdlevent. keycode = Sdlkeycode.Right; _} ->
    `Continue {t with events = Set.remove t.events `Right; }
  | _ ->
    `Continue t
;;

let on_step t =
  let panning_x =
    t.panning_x
    +. match (Set.mem t.events `Left, Set.mem t.events `Right) with
    | (true, false) -> 0.0 -. 0.5
    | (false, true) -> 0.5
    | _             -> 0.0 -. t.panning_x /. 8.0
  in
  let camera_x = t.camera_x +. panning_x in
  let panning_y =
    t.panning_y
    +. match (Set.mem t.events `Up, Set.mem t.events `Down) with
    | (true, false) -> 0.0 -. 0.5
    | (false, true) -> 0.5
    | _             -> 0.0 -. t.panning_y /. 8.0
  in
  let camera_y = t.camera_y +. panning_y in
  let t = { t with panning_x; camera_x; panning_y; camera_y; } in
  `Continue t
;;

let edit ~file ~data_dir =
  load ~file
  >>= fun world ->
  let module W = (val world : World.S) in
  Game.with_sdl ~data_dir ~f:(fun ~ctx ~width ~height ->
      (* CR scvalex: This shouldn't be W.create; it should be some
         static, auto-generated submodule. *)
      let world_state = W.create ~width ~height in
      let initial_state = create () in
      let steps_per_sec = 60.0 in
      let drawing_of_state t =
        World.to_drawing (W.entities world_state) ~layers:W.layers
          ~camera:(`X t.camera_x, `Y t.camera_y)
      in
      Game.main_loop ~initial_state ~on_event ~on_step
        ~steps_per_sec ~drawing_of_state ~ctx)
;;
