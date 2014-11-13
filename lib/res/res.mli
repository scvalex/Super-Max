open Core.Std
open Async.Std

module Mesh : sig
  type t

  val vertices : t -> Float_array.t

  val create :
    vertices : Float_array.t
    -> t
end

type t =
  | Mesh of Mesh.t

val save : t -> string -> unit Deferred.Or_error.t

val load : string -> t Deferred.Or_error.t
