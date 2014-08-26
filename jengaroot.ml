open Core.Std
open Async.Std
open Jenga_lib.Api

let env =
  Env.create []
;;

let setup () =
  Deferred.return env
;;
