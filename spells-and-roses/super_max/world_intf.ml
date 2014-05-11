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

  module World_editor_private : sig
    val entities : ((entity_common, world) Entity.t * Position.t) Entity.Id.Map.t
  end

  val layers : int list
end

let univ_constr : (module S) Ocaml_dynloader.Univ_constr.t =
  Ocaml_dynloader.Univ_constr.create ()
;;
