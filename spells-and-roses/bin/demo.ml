open Core.Std let _ = _squelch_unused_module_warning_
open Super_max.Std
open World_intf.Zelda

type engine = unit

type t = {
  width    : int;
  height   : int;
  entities : (engine Entity.t * Position.t) Entity.Id.Map.t;
} with fields

let steps_per_sec = 60.0;;

let create ~width ~height =
  let entities = Entity.Id.Map.empty in
  { width; height; entities; }
;;

let drawing_of_state t =
  Drawing.Example.rectangles ~width:t.width ~height:t.height
;;

let on_step t =
  `Continue t
;;

let on_event t _event =
  `Continue t
;;
