open Core.Std
open Async.Std

module Id : sig
  type t

  val to_string : t -> string

  val name : t -> string

  val pack  : t -> string

  val create :
    pack : string
    -> name : string
    -> t
end

val add_pack :
  dir : string
  -> unit

val load :
  id : Id.t
  -> cache_until : [`Don't_cache | `End_of_days]
  -> Res.t Deferred.Or_error.t
