open Core.Std let _ = _squelch_unused_module_warning_
open Async.Std
open Jenga_lib.Api

let recusive_default_scheme ~dir =
  Scheme.rules [
    Rule.default ~dir [
      let echo str =
        Action.shell ~dir ~prog:"/bin/echo" ~args:[str]
      in
      Dep.action (Dep.return (echo "Hello, Jenga"))
    ]
  ]
;;

let scheme ~dir =
  Scheme.all
    [ recusive_default_scheme ~dir
    ]
;;

let setup () =
  let env =
    Env.create (fun ~dir -> scheme ~dir)
  in
  Deferred.return env
;;
