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
      | `Place
      ] with sexp, compare
  end
  include T
  include Comparable.Make(T)
end

module Ui(W : World.S) = struct
  let (s_width, s_height) = W.sprite_size;;
  let s_width = Float.of_int s_width;;
  let s_height = Float.of_int s_height;;

  type t = {
    focus_i            : int;
    focus_j            : int;
    focus_x            : float;
    focus_y            : float;
    events             : Event.Set.t;
    entities           : ((W.entity_common, W.world) Entity.t * Position.t) Entity.Id.Map.t;
    width              : float;
    height             : float;
    available_entities : (string * Drawing.t) array;
    selected_entity    : int;
  }

  let create ~width ~height =
    let focus_i = 0 in
    let focus_j = 0 in
    let focus_x = 0.0 in
    let focus_y = 0.0 in
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
    { focus_x; focus_y; events; entities;
      width; height; available_entities; selected_entity;
      focus_i; focus_j;
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
      ; (Sdlkeycode.Space, `Place)
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

  let add_entity_if_unique ~entities ~loc_i ~loc_j ~layer creator =
    let y = Float.of_int loc_i *. s_height in
    let x = Float.of_int loc_j *. s_width in
    let z = layer in
    if Map.exists entities ~f:(fun (_, pos) ->
        Float.(pos.Position.x = x && pos.Position.y = y))
    then begin
      entities
    end else begin
      let entity = creator () in
      Map.add entities ~key:(Entity.id entity)
        ~data:(entity, { Position. x; y; z; })
    end
  ;;

  let on_step t =
    let handle_pan ~target ~size ~focus ~down ~up =
      if Float.(abs ((of_int target *. size) -. focus) > size /. 2.0)
      then begin
        target
      end else begin
        target
        + match (Set.mem t.events down, Set.mem t.events up) with
        | (true, false) -> 1
        | (false, true) -> 0 - 1
        | _             -> 0
      end
    in
    let updated_focus ~target ~size ~focus =
      focus +. ((Float.of_int target *. size) -. focus) /. 8.0
    in
    let focus_j =
      handle_pan ~target:t.focus_j ~size:s_width ~focus:t.focus_x
        ~down:`Left ~up:`Right
    in
    let focus_x =
      updated_focus ~target:focus_j ~size:s_width ~focus:t.focus_x
    in
    let focus_i =
      handle_pan ~target:t.focus_i ~size:s_height ~focus:t.focus_y
        ~down:`Up ~up:`Down
    in
    let focus_y =
      updated_focus ~target:focus_i ~size:s_height ~focus:t.focus_y
    in
    let entities =
      if Set.mem t.events `Place
      then begin
        let (kind, _) = t.available_entities.(t.selected_entity) in
        let creator = Map.find_exn W.entity_creators kind in
        (* CR scvalex: Choose active layer. *)
        add_entity_if_unique ~entities:t.entities
          ~loc_i:focus_i ~loc_j:focus_j ~layer:2
          creator
      end else begin
        t.entities
      end
    in
    let t = { t with focus_i; focus_j; focus_x; focus_y; entities; } in
    `Continue t
  ;;

  let drawing_of_state t =
    let open Drawing in
    let text = text ~font:"UbuntuMono-B.ttf" in
    let camera_x = t.focus_x -. t.width /. 2.0 in
    let camera_y = t.focus_y -. t.height /. 2.0 in
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
        text ~size_pt:24 ~position:(`X `Centre, `Y `Top) kind
      in
      translate ~x:50.0 ~y:90.0
        (many [ translate ~x:(s_width /. 2.0) ~y:(s_height +. 10.0)
                  label
              ; drawing
              ])
    in
    let coordinates =
      translate ~x:30.0 ~y:30.0
        (text ~size_pt:24 (sprintf "%+d, %+d" t.focus_i t.focus_j))
    in
    many
      [ world_drawing
      ; focused_border
      ; selected_entity
      ; coordinates
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
