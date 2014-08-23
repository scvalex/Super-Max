open Core.Std

include World_intf

(* CR scvalex: Use Core.Sequence instead of allocating intermediate
   lists. *)
let to_drawing entities ~layers ~camera:(`X camera_x, `Y camera_y) =
  let open Drawing in
  let camera_translate ~pos drawing =
    translate drawing
      ~x:(pos.Position.x -. camera_x) ~y:(pos.Position.y -. camera_y)
  in
  let entities = Map.data entities in
  let draw_layer current_layer _name =
    (* CR scvalex: The in-layer drawing order is
       left-right-top-down. *)
    many
      (List.filter_map entities ~f:(fun (entity, pos) ->
         if Int.(pos.Position.z = current_layer)
         then begin
           Some (camera_translate ~pos
                   (Entity.to_drawing entity))
         end else begin
           None
         end))
  in
  many (List.mapi layers ~f:draw_layer)
;;
