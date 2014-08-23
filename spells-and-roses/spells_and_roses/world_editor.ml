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

let font = "UbuntuMono-B.ttf";;

module Event = struct
  module T = struct
    type t = [ `Up | `Down | `Left | `Right ] with sexp, compare
  end

  include T
  include Comparable.Make(T)
end

module Ui(W : World.S) = struct
  let (s_width, s_height) = W.sprite_size;;
  let s_width = Float.of_int s_width;;
  let s_height = Float.of_int s_height;;
  let w_layers = Array.of_list W.layers;;

  module Entity_manager = struct
    let add_unique ~entities ~loc_i ~loc_j ~layer creator =
      let y = Float.of_int loc_i *. s_height in
      let x = Float.of_int loc_j *. s_width in
      let z = layer in
      if Map.exists entities ~f:(fun (_, pos) ->
          Float.(pos.Position.x = x && pos.Position.y = y)
          && Int.(pos.Position.z = z))
      then begin
        entities
      end else begin
        let entity = creator () in
        Map.add entities ~key:(Entity.id entity)
          ~data:(entity, { Position. x; y; z; })
      end
    ;;

    let remove ~entities ~loc_i ~loc_j ~layer =
      let y = Float.of_int loc_i *. s_height in
      let x = Float.of_int loc_j *. s_width in
      let z = layer in
      Map.filter entities ~f:(fun ~key:_ ~data:(_, pos) ->
          not (Float .(pos.Position.x = x && pos.Position.y = y)
               && Int.(pos.Position.z = z)))
    ;;
  end

  module Map_editor = struct
    type t = {
      focus_i            : int;
      focus_j            : int;
      focus_x            : float;
      focus_y            : float;
      active_events      : Event.Set.t;
      entities           : ((W.common, W.event) Entity.t * Position.t) Entity.Id.Map.t;
      width              : float;
      height             : float;
      available_entities : (string * Drawing.t) array;
      selected_entity    : int;
      selected_layer     : int;
    }

    let create ~width ~height =
      let focus_i = 0 in
      let focus_j = 0 in
      let focus_x = 0.0 in
      let focus_y = 0.0 in
      let active_events = Event.Set.empty in
      let entities = W.World_editor_private.entities in
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
      let selected_layer = 0 in
      { focus_x; focus_y; active_events; entities;
        width; height; available_entities; selected_entity;
        focus_i; focus_j; selected_layer;
      }
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
        t
      | None ->
        match ev with
        | Sdlevent.KeyUp {Sdlevent. keycode = Sdlkeycode.Tab; _} ->
          let selected_entity =
            let total_entities = Map.length W.entity_creators in
            Int.((t.selected_entity + 1) mod total_entities)
          in
          { t with selected_entity; }
        | Sdlevent.KeyUp {Sdlevent. keycode = Sdlkeycode.PageUp; _} ->
          let selected_layer =
            let len = Array.length w_layers in
            Int.((t.selected_layer + 1) mod len)
          in
          { t with selected_layer; }
        | Sdlevent.KeyUp {Sdlevent. keycode = Sdlkeycode.PageDown; _} ->
          let selected_layer =
            let len = Array.length w_layers in
            Int.((t.selected_layer + len - 1) mod len)
          in
          { t with selected_layer; }
        | Sdlevent.KeyUp {Sdlevent. keycode = Sdlkeycode.Space; _} ->
          let entities =
            let (kind, _) = t.available_entities.(t.selected_entity) in
            let creator = Map.find_exn W.entity_creators kind in
            Entity_manager.add_unique ~entities:t.entities
              ~loc_i:t.focus_i ~loc_j:t.focus_j ~layer:t.selected_layer
              creator
          in
          { t with entities; }
        | Sdlevent.KeyUp {Sdlevent. keycode = Sdlkeycode.Delete; _}
        | Sdlevent.KeyUp {Sdlevent. keycode = Sdlkeycode.Backspace; _} ->
          let entities =
            Entity_manager.remove ~entities:t.entities
              ~loc_i:t.focus_i ~loc_j:t.focus_j ~layer:t.selected_layer
          in
          { t with entities; }
        | _ ->
          t
    ;;

    let on_step t =
      let handle_pan ~target ~size ~focus ~down ~up =
        if Float.(abs ((of_int target *. size) -. focus) > size /. 2.0)
        then begin
          target
        end else begin
          target
          + match (Set.mem t.active_events down, Set.mem t.active_events up) with
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
          ~down:`Right ~up:`Left
      in
      let focus_x =
        updated_focus ~target:focus_j ~size:s_width ~focus:t.focus_x
      in
      let focus_i =
        handle_pan ~target:t.focus_i ~size:s_height ~focus:t.focus_y
          ~down:`Down ~up:`Up
      in
      let focus_y =
        updated_focus ~target:focus_i ~size:s_height ~focus:t.focus_y
      in
      let t =
        { t with focus_i; focus_j; focus_x; focus_y; }
      in
      t
    ;;

    let to_drawing t =
      let open Drawing in
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
      let layout_vertically ~x ~y_step drawings =
        List.mapi drawings ~f:(fun idx drawing ->
          translate ~x ~y:(Float.of_int idx *. y_step)
            drawing)
      in
      let tools =
        let coordinates =
          text ~font ~size_pt:24 ~position:(`X `Centre, `Y `Top)
            [sprintf "%+d, %+d" t.focus_i t.focus_j]
        in
        let selected_layer =
          text ~font ~size_pt:24 ~position:(`X `Centre, `Y `Top)
            [w_layers.(t.selected_layer)]
        in
        let selected_entity =
          let (kind, drawing) = t.available_entities.(t.selected_entity) in
          let label =
            text ~font ~size_pt:24 ~position:(`X `Centre, `Y `Top) [kind]
          in
          translate ~x:(0.0 -. s_width /. 2.0) ~y:0.0
            (many [ translate ~x:(s_width /. 2.0) ~y:(s_height +. 10.0)
                      label
                  ; drawing
                  ])
        in
        layout_vertically ~x:80.0 ~y_step:40.0
          [ coordinates; selected_layer; selected_entity; ]
      in
      many
        ([ world_drawing; focused_border; ]
         @ tools)
    ;;
  end

  module Quit_dialog = struct
    type t = [`Y | `N] Dialog.Simple_query.t

    let width = 400.0;;

    let height = 200.0;;

    let create () =
      Dialog.Simple_query.create
        ~width
        ~height
        ~font
        ~size_pt:24
        ~event_map:[(Sdlkeycode.Y, `Y); (Sdlkeycode.N, `N)]
        ~query:"Really quit?"
    ;;
  end

  type t = {
    editor      : Map_editor.t;
    focused     : [ `Editor | `Quit_dialog of Quit_dialog.t];
    width       : float;
    height      : float;
  }

  let show_quit_dialog t =
    { t with focused = `Quit_dialog (Quit_dialog.create ()); }
  ;;

  let hide_quit_dialog t =
    { t with focused = `Editor; }
  ;;

  let on_event t ~engine ev =
    match ev with
    | Sdlevent.Quit _
    | Sdlevent.KeyUp {Sdlevent. keycode = Sdlkeycode.Q; _} ->
      show_quit_dialog t
    | _ ->
      match t.focused with
      | `Editor ->
        let editor = Map_editor.on_event t.editor ev in
        { t with editor; }
      | `Quit_dialog quit_dialog ->
        match Dialog.Simple_query.on_event quit_dialog ev with
        | None    -> t
        | Some `N -> hide_quit_dialog t
        | Some `Y -> Engine.quit engine; t
  ;;

  let on_step t ~engine:_ =
    let editor = Map_editor.on_step t.editor in
    { t with editor; }
  ;;

  let to_drawing t =
    let open Drawing in
    let quit_dialog_drawing =
      match t.focused with
      | `Quit_dialog quit_dialog ->
        [translate
           (Dialog.Simple_query.to_drawing quit_dialog)
           ~x:((t.width -. Quit_dialog.width) /. 2.0)
           ~y:((t.height -. Quit_dialog.height) /. 2.0)]
      | _ ->
        []
    in
    many
      (List.concat
         [ [Map_editor.to_drawing t.editor]
         ; quit_dialog_drawing
         ])
  ;;

  module World_editor_game = struct
    include Game.No_multiplayer

    type nonrec t = t

    let create ~width ~height =
      let width = Float.of_int width in
      let height = Float.of_int height in
      let editor = Map_editor.create ~width ~height in
      let focused = `Editor in
      { editor; focused; width; height; }
    ;;

    let steps_per_second = 60.0;;

    let on_event = on_event;;

    let on_step = on_step;;

    let to_drawing = to_drawing;;
  end

  let run ~data_dir =
    Game.run (module World_editor_game) ~data_dir
  ;;
end

let edit ~file ~data_dir =
  load ~file
  >>= fun world ->
  let module W = (val world : World.S) in
  let module Ui = Ui(W) in
  Ui.run ~data_dir
;;
