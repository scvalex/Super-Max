open Core.Std
open Async.Std
open Ocaml_plugin.Std

module Position = World.Position

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

module Ui(W : World.S) = struct
  type t = {
    camera_x  : float;
    camera_y  : float;
    panning_x : float;
    panning_y : float;
    events    : Event.Set.t;
    entities  : ((W.entity_common, W.world) Entity.t * Position.t) Entity.Id.Map.t;
  }

  let create () =
    let camera_x = 0.0 in
    let camera_y = 0.0 in
    let panning_x = 0.0 in
    let panning_y = 0.0 in
    let events = Event.Set.empty in
    let entities = W.World_editor_private.entities in
    { camera_x; camera_y; panning_x; panning_y; events; entities; }
  ;;

  let generic_handle_key_event t ~key ~event ev =
    match ev with
    | Sdlevent.KeyDown {Sdlevent. keycode; _} when key = keycode ->
      Some {t with events = Set.add t.events event; }
    | Sdlevent.KeyUp {Sdlevent. keycode; _} when key = keycode ->
      Some {t with events = Set.remove t.events event; }
    | _ ->
      None
  ;;

  let on_event t ev =
    let keys_events =
      [ (Sdlkeycode.Down, `Down)
      ; (Sdlkeycode.Up, `Up)
      ; (Sdlkeycode.Left, `Left)
      ; (Sdlkeycode.Right, `Right)
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

  let on_step t =
    let handle_pan ~camera ~panning ~down ~up =
      let panning =
        panning
        +. match (Set.mem t.events down, Set.mem t.events up) with
        | (true, false) -> 0.0 -. 0.5
        | (false, true) -> 0.5
        | _             -> 0.0 -. panning /. 8.0
      in
      let camera = camera +. panning in
      (`Panning panning, `Camera camera)
    in
    let (`Panning panning_x, `Camera camera_x) =
      handle_pan ~camera:t.camera_x ~panning:t.panning_x
        ~down:`Left ~up:`Right
    in
    let (`Panning panning_y, `Camera camera_y) =
      handle_pan ~camera:t.camera_y ~panning:t.panning_y
        ~down:`Up ~up:`Down
    in
    let t = { t with panning_x; camera_x; panning_y; camera_y; } in
    `Continue t
  ;;

  let drawing_of_state t =
    let open Drawing in
    let world_drawing =
      World.to_drawing t.entities ~layers:W.layers
        ~camera:(`X t.camera_x, `Y t.camera_y)
    in
    many
      [ world_drawing
      ]
  ;;

  let run ~data_dir =
    let t = create () in
    Game.with_sdl ~data_dir ~f:(fun ~ctx ~width:_ ~height:_ ->
        let initial_state = t in
        let steps_per_sec = 60.0 in
        Game.main_loop ~initial_state ~on_event ~on_step
          ~steps_per_sec ~drawing_of_state ~ctx)
  ;;
end

let edit ~file ~data_dir =
  load ~file
  >>= fun world ->
  let module W = (val world : World.S) in
  let module Ui = Ui(W) in
  Ui.run ~data_dir
;;