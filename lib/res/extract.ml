(* open Core.Std *)
(* open Async.Std *)

let extract_mesh ~source ~geometry_id ~target =
  let res =
    Res.create_mesh ~source ~geometry_id ~vertices:(Float_array.create 0) ()
  in
  Res.save res target
;;
