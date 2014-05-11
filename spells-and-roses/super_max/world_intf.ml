open Core.Std
open Ocaml_plugin.Std

module Position = struct
  type t = {
    x : float;
    y : float;
    z : int;
  } with sexp
end

module type S = sig
  type world
  type entity_common
  type t

  val entities : t -> ((entity_common, world) Entity.t * Position.t) Entity.Id.Map.t

  val layers : int list
end

(* CR scvalex: Use Core.Sequence instead of allocating intermediate
   lists. *)
let to_drawing entities ~layers ~camera:(`X camera_x, `Y camera_y) =
  let open Drawing in
  let camera_translate ~pos drawing =
    translate drawing
      ~x:(pos.Position.x -. camera_x) ~y:(pos.Position.y -. camera_y)
  in
  let entities = Map.data entities in
  let draw_layer current_layer =
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
  many (List.map layers ~f:draw_layer)
;;

let univ_constr : (module S) Ocaml_dynloader.Univ_constr.t =
  Ocaml_dynloader.Univ_constr.create ()
;;
