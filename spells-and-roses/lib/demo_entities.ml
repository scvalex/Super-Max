open Core.Std
open Super_max.Std

type world = unit

let sprite_width = 40;;
let sprite_height = 40;;

let static_entities_counter = ref 0;;

module Make_static(E : sig
              val name : string
              val sprite_sheet : string
              val src_x : int
              val src_y : int
           end) = struct
  let drawing =
    Drawing.image E.sprite_sheet
      ~clip:(`X E.src_x,
             `Y E.src_y,
             `Width sprite_width,
             `Height sprite_height)
  ;;

  let to_drawing () _ = drawing;;

  let on_step () world = ((), world);;

  let on_event () world _ev = ((), world);;

  let create () =
    let id =
      Entity.Id.of_string (sprintf "static-%s-%d" E.name !static_entities_counter)
    in
    incr static_entities_counter;
    let state = () in
    Entity.create ~id ~to_drawing ~on_step ~on_event ~state
  ;;
end

module Grass = Make_static(struct
  let name = "grass";;
  let sprite_sheet = "demo_sheet.png";;
  let src_x = 0;;
  let src_y = 40;;
end)

let create_world () =
  ()
;;
