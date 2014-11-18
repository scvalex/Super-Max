open Core.Std
open Async.Std

module Mesh : sig
  type t

  val positions : t -> Float_array.t
end

module Program : sig
  type t

  val vertex : t -> string

  val fragment : t -> string
end

type t

val save : t -> string -> unit Deferred.Or_error.t

val load : string -> t Deferred.Or_error.t

val data : t -> [`Mesh of Mesh.t | `Program of Program.t]

val create_mesh :
  ?source : string
  -> ?source_id : string
  -> positions : Float_array.t
  -> unit
  -> t

val create_program :
  vertex : string
  -> fragment : string
  -> unit
  -> t

val metadata : t -> string
