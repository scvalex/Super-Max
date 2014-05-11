open Core.Std let _ = _squelch_unused_module_warning_
open Super_max.Std
open World_intf.Zelda

module D = Demo_entities
module P = Position

type world = Demo_entities.world

type t = {
  width    : int;
  height   : int;
  camera_x : float;
  camera_y : float;
  world    : world;
  entities : (world Entity.t * Position.t) Entity.Id.Map.t;
} with fields

let steps_per_sec = 60.0;;

let layers = [0; 1; 2; 3];;

let pos ~x ~y ~z =
  let z =
    match z with
    | `Parallax   -> 0
    | `Background -> 1
    | `Scene      -> 2
    | `Foreground -> 3
  in
  { Position. x; y; z; }
;;

let add_entitiy entities entity ~pos =
  Map.add entities ~key:(Entity.id entity) ~data:(entity, pos)
;;

let create ~width ~height =
  let entities = Entity.Id.Map.empty in
  let entities =
    let grass = Demo_entities.Grass.create () in
    add_entitiy entities grass ~pos:(pos ~x:0.0 ~y:0.0 ~z:`Background)
  in
  let entities =
    let cliff = Demo_entities.Cliff_s.create () in
    add_entitiy entities cliff ~pos:(pos ~x:40.0 ~y:40.0 ~z:`Background)
  in
  let camera_x = 0.0 in
  let camera_y = 0.0 in
  let world = D.create_world () in
  { width; height; camera_x; camera_y; world; entities; }
;;

(* CR scvalex: Use Core.Sequence instead of allocating intermediate
   lists. *)
let drawing_of_state t =
  let open Drawing in
  let camera_translate ~pos drawing =
    translate ~x:(pos.P.x -. t.camera_x) ~y:(pos.P.y -. t.camera_y)
      drawing
  in
  let entities = Map.data t.entities in
  let draw_layer current_layer =
    (* CR scvalex: The in-layer drawing order is
       left-right-top-down. *)
    many
      (List.filter_map entities ~f:(fun (entity, pos) ->
           if Int.(pos.P.z = current_layer)
           then begin
             Some (camera_translate ~pos
                     (Entity.to_drawing entity t.world))
           end else begin
             None
           end))
  in
  many (List.map layers ~f:draw_layer)
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
