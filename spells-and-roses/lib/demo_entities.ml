open Core.Std

type world = unit

type common = {
  obstacle : bool;
} with fields

let sprite_width = 40;;
let sprite_height = 40;;

let static_entities_counter = ref 0;;

module Make_static(E : sig
              val name : string
              val sprite_sheet : string
              val src_x : int
              val src_y : int
              val obstacle : bool
           end) = struct
  let drawing =
    Drawing.image E.sprite_sheet
      ~clip:(`X E.src_x,
             `Y E.src_y,
             `Width sprite_width,
             `Height sprite_height)
  ;;

  let to_drawing _state = drawing;;

  let on_step state world = (state, world);;

  let on_event state world _ev = (state, world);;

  let create () =
    let id =
      Entity.Id.of_string (sprintf "static-%s-%d" E.name !static_entities_counter)
    in
    incr static_entities_counter;
    let state = { obstacle = E.obstacle; } in
    Entity.create ~id ~to_drawing ~on_step ~on_event ~state ~common:Fn.id
  ;;
end

module Grass = Make_static(struct
  let name = "grass";;
  let sprite_sheet = "demo_sheet.png";;
  let src_x = 0 * sprite_width;;
  let src_y = 1 * sprite_height;;
  let obstacle = false;;
end)

module Cliff_s = Make_static(struct
  let name = "cliff_s";;
  let sprite_sheet = "demo_sheet.png";;
  let src_x = 1 * sprite_width;;
  let src_y = 1 * sprite_height;;
  let obstacle = true;;
end)

let create_world () =
  ()
;;

let is_obstacle entity =
  (Entity.common entity).obstacle
;;
