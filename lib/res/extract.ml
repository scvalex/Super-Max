(* open Core.Std *)
(* open Async.Std *)

let extract_mesh ~source:_ ~geometry_id:_ ~target =
  let res = Res.Mesh (Res.Mesh.create ~vertices:(Float_array.create 0)) in
  Res.save res target
;;
