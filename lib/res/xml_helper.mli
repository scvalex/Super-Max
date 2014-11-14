open Core.Std
open Async.Std

type t = Xml.xml

val parse_file : string -> t Deferred.Or_error.t

val children : t -> t list

val tag : t -> string option

module Query : sig
  type t
  type one

  val empty : t

  val (+>) : t -> one -> t

  val tag : string -> one

  val attr : string -> string -> one
end

val query : t -> Query.t -> t list
