open Core.Std let _ = _squelch_unused_module_warning_
open Async.Std
open Jenga_lib.Api

let ( *>>= ) = Dep.bind;;

let ( *>>| ) = Dep.map;;

let message str =
  print_endline ("!!jengaroot.ml: " ^ str)
;;
let _ = message;;

let (^/) dir name = Path.relative ~dir name;;

let (/^) = (^/);;

let basename = Path.basename;;

let nothing_to_build_rules ~dir =
  Dep.return [Rule.default ~dir []]
;;

let everything_under_targets ~dir ~subdirs =
  let subdirs =
    List.map subdirs ~f:(fun subdir ->
      dir ^/ subdir)
  in
  Dep.List.concat_map subdirs ~f:(fun subdir ->
    Dep.subdirs ~dir:subdir)
;;
