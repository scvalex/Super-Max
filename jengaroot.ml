open Core.Std let _ = _squelch_unused_module_warning_
open Async.Std
open Jenga_lib.Api

let env =
  Env.create
    (fun ~dir:_ ->
       Scheme.switch_glob
         [])
;;

let setup () =
  Deferred.return env
;;
