open Core.Std let _ = _squelch_unused_module_warning_
open Async.Std let _ = _squelch_unused_module_warning_

let extract_mesh ~source ~source_id ~target_id =
  let target = target_id ^ ".res" in
  let res =
    Res.create_mesh ~source ~source_id ~vertices:(Float_array.create 0) ()
  in
  Res.save res target
;;
