open Core.Std

module Id : Identifiable

module Mesh : sig
  type t with sexp

  val id : t -> Id.t

  val file : t -> string

  val geometry_id : t -> string
end

type t =
  | Mesh of Mesh.t
with sexp
