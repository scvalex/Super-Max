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
    focus_x            : float;
    focus_y            : float;
    panning_x          : float;
    panning_y          : float;
    events             : Event.Set.t;
    entities           : ((W.entity_common, W.world) Entity.t * Position.t) Entity.Id.Map.t;
    width              : float;
    height             : float;
    available_entities : (string * Drawing.t) array;
    selected_entity    : int;
  }

  let create ~width ~height =
    let focus_x = 0.0 in
    let focus_y = 0.0 in
    let panning_x = 0.0 in
    let panning_y = 0.0 in
    let events = Event.Set.empty in
    let entities = W.World_editor_private.entities in
    let width = Float.of_int width in
    let height = Float.of_int height in
    let available_entities =
      Array.of_list
        (List.map (Map.to_alist W.entity_creators) ~f:(fun (kind, create) ->
             (kind, Entity.to_drawing (create ()))))
    in
    let selected_entity =
      if Map.is_empty W.entity_creators
      then failwith "No available entities in world"
      else 0
    in
    { focus_x; focus_y; panning_x; panning_y; events; entities;
      width; height; available_entities; selected_entity;
    }
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
      | Sdlevent.KeyUp {Sdlevent. keycode = Sdlkeycode.Tab; _} ->
        let selected_entity =
          let total_entities = Map.length W.entity_creators in
          Int.((t.selected_entity + 1) mod total_entities)
        in
        `Continue { t with selected_entity; }
      | _ ->
        `Continue t
  ;;

  let on_step t =
    let handle_pan ~focus ~panning ~down ~up =
      let panning =
        panning
        +. match (Set.mem t.events down, Set.mem t.events up) with
        | (true, false) -> 0.0 -. 0.5
        | (false, true) -> 0.5
        | _             -> 0.0 -. panning /. 8.0
      in
      let focus = focus +. panning in
      (`Panning panning, `Focus focus)
    in
    let (`Panning panning_x, `Focus focus_x) =
      handle_pan ~focus:t.focus_x ~panning:t.panning_x
        ~down:`Left ~up:`Right
    in
    let (`Panning panning_y, `Focus focus_y) =
      handle_pan ~focus:t.focus_y ~panning:t.panning_y
        ~down:`Up ~up:`Down
    in
    let t = { t with panning_x; focus_x; panning_y; focus_y; } in
    `Continue t
  ;;

  let drawing_of_state t =
    let open Drawing in
    let (s_width, s_height) = W.sprite_size in
    let s_width = Float.of_int s_width in
    let s_height = Float.of_int s_height in
    let camera_x = (t.focus_x -. t.width) /. 2.0 in
    let camera_y = (t.focus_y -. t.height) /. 2.0 in
    let world_drawing =
      World.to_drawing t.entities ~layers:W.layers
        ~camera:(`X camera_x, `Y camera_y)
    in
    let focused_border =
      colour ~r:0.9 ~g:0.3 ~b:0.3
        (translate ~x:(t.width /. 2.0) ~y:(t.height /. 2.0)
           (rectangle ~width:s_width ~height:s_height ~filled:false))
    in
    let selected_entity =
      let (kind, drawing) = t.available_entities.(t.selected_entity) in
      let label =
        text ~font:"UbuntuMono-B.ttf" ~size_pt:24
          ~position:(`X `Centre, `Y `Top)
          kind
      in
      translate ~x:30.0 ~y:(t.height /. 6.0)
        (many [ translate ~x:(s_width /. 2.0) ~y:(s_height +. 10.0)
                  label
              ; drawing
              ])
    in
    many
      [ world_drawing
      ; focused_border
      ; selected_entity
      ]
  ;;

  let run ~data_dir =
    Game.with_sdl ~data_dir ~f:(fun ~ctx ~width ~height ->
        let t = create ~width ~height in
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
