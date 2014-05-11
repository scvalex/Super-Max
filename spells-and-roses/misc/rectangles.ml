open Core.Std let _ = _squelch_unused_module_warning_

let run ~args =
  let data_dir =
    match args with
    | (resources :: _) -> resources
    | []               -> "resources"
  in
  Tests.Rectangles.run ~data_dir
;;
