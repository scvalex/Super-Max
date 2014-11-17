open Core.Std
open Async.Std

module Mesh : sig
  type t

  val positions : t -> Float_array.t
end

type t

val save : t -> string -> unit Deferred.Or_error.t

val load : string -> t Deferred.Or_error.t

val create_mesh :
  ?source : string
  -> ?source_id : string
  -> positions : Float_array.t
  -> unit
  -> t

val metadata : t -> string
