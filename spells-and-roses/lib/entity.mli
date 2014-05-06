open Core.Std

module Id : Identifiable.S

module On_disk : sig
  type t

  include Sexpable.S with type t := t
end

type t

val create :
    id : ('a -> Id.t)
 -> to_drawing : ('a -> Drawing.t)
 -> on_step : ('a -> 'a)
 -> on_event : ('a -> Sdlevent.t -> 'a)
 -> state : 'a
 -> t

val id : t -> Id.t

val to_drawing : t -> Drawing.t

val on_step : t -> t

val on_event : t -> Sdlevent.t -> t
