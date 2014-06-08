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
  type event
  type common
  type t

  module World_editor_private : sig
    val entities : ((common, event) Entity.t * Position.t) Entity.Id.Map.t
  end

  val layers : string list

  val sprite_size : (int * int)

  val entity_creators : (unit -> (common, event) Entity.t) String.Map.t
end

let univ_constr : (module S) Ocaml_dynloader.Univ_constr.t =
  Ocaml_dynloader.Univ_constr.create ()
;;
