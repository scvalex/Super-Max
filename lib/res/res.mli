open Core.Std
open Async.Std

module Mesh : sig
  type t

  val id : t -> Res_id.t

  val positions : t -> Float_array.t

  val indices : t -> Int_array.t
end

module Program : sig
  type t

  val id : t -> Res_id.t

  val vertex : t -> string

  val fragment : t -> string
end

type t

val save : t -> string -> unit Deferred.Or_error.t

val load :
  id : Res_id.t
  -> string
  -> t Deferred.Or_error.t

val data : t -> [`Mesh of Mesh.t | `Program of Program.t]

val id : t -> Res_id.t

val create_mesh :
  ?source : string
  -> ?source_id : string
  -> positions : Float_array.t
  -> indices : Int_array.t
  -> Res_id.t
  -> t

val create_program :
  vertex : string
  -> fragment : string
  -> Res_id.t
  -> t

val metadata : t -> string
