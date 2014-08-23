open Core.Std

module D = Demo_entities
module Pos = World.Position

type common = Demo_entities.common
type event = Sdlevent.t

let sprite_size =
  Demo_entities.(sprite_width, sprite_height)
;;

let entity_creators = Demo_entities.entity_creators;;

type t = {
  width    : int;
  height   : int;
  camera_x : float;
  camera_y : float;
  entities : ((common, Sdlevent.t) Entity.t * Pos.t) Entity.Id.Map.t;
} with fields

let add_entitiy entities entity ~pos =
  Map.add entities ~key:(Entity.id entity) ~data:(entity, pos)
;;

let pos ~x ~y ~z =
  let z =
    match z with
    | `Parallax   -> 0
    | `Background -> 1
    | `Scene      -> 2
    | `Foreground -> 3
  in
  { Pos. x; y; z; }
;;

let layers = ["parallax"; "background"; "scene"; "foreground"];;

module World_editor_private = struct
  let entities =
    let acc = Entity.Id.Map.empty in
    let acc =
      let grass = Demo_entities.Grass.create () in
      add_entitiy acc grass ~pos:(pos ~x:0.0 ~y:0.0 ~z:`Background)
    in
    let acc =
      let cliff = Demo_entities.Cliff_s.create () in
      add_entitiy acc cliff ~pos:(pos ~x:0.0 ~y:40.0 ~z:`Background)
    in
    acc
  ;;
end

include Game.No_multiplayer

let steps_per_second = 60.0;;

let create ~width ~height =
  let entities = World_editor_private.entities in
  let camera_x = 0.0 in
  let camera_y = 0.0 in
  { width; height; camera_x; camera_y; entities; }
;;

(* CR scvalex: Use Core.Sequence instead of allocating intermediate
   lists. *)
let to_drawing t =
  World.to_drawing t.entities ~layers
    ~camera:(`X t.camera_x, `Y t.camera_y)
;;

let on_step _t ~engine:_ =
  ()
;;
let on_event _t ~engine ev =
  match ev with
  | Sdlevent.Quit _
  | Sdlevent.KeyUp {Sdlevent. keycode = Sdlkeycode.Q; _} ->
    Engine.quit engine
  | _ ->
    ()
;;
