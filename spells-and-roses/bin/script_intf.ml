open Async.Std
open Ocaml_plugin.Std

module type S = sig
  val run :
       args : string list
    -> unit Deferred.t
end

let univ_constr : (module S) Ocaml_dynloader.Univ_constr.t =
  Ocaml_dynloader.Univ_constr.create ()
;;
