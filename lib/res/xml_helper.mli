open Core.Std
open Async.Std

type t = Xml.xml

val parse_file_exn : string -> t Deferred.t

val children : t -> t list

val tag : t -> string option

val attr : t -> string -> string option

val attr_exn :
  here : Source_code_position.t
  -> t
  -> string
  -> string

val text_content_exn :
  here : Source_code_position.t
  -> t
  -> string

module Query : sig
  type t
  type one

  val empty : t

  val (+>) : t -> one -> t

  val tag : string -> one

  val with_attr : one -> string -> string -> one
end

val matches : t -> Query.t -> t list

val match_one_exn :
  here : Source_code_position.t
  -> t
  -> Query.t
  -> t

val exists : t -> Query.t -> bool
