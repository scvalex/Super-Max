open Core.Std
open Async.Std

module Mesh : sig
  type t

  val vertices : t -> Float_array.t
end

type t

val save : t -> string -> unit Deferred.Or_error.t

val load : string -> t Deferred.Or_error.t

val create_mesh :
  ?source : string
  -> ?geometry_id : string
  -> vertices : Float_array.t
  -> unit
  -> t

val metadata : t -> string
