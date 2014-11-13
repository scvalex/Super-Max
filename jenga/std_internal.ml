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
