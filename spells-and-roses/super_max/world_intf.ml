open Core.Std
open Ocaml_plugin.Std

module Position = struct
  type t = {
    x : float;
    y : float;
    z : int;
  } with sexp
end

module type S = sig
  type world
  type entity_common
  type t

  val create :
       width : int
    -> height : int
    -> t

  val entities : t -> ((entity_common, world) Entity.t * Position.t) Entity.Id.Map.t

  val layers : int list
end

let univ_constr : (module S) Ocaml_dynloader.Univ_constr.t =
  Ocaml_dynloader.Univ_constr.create ()
;;
