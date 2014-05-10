open Core.Std

module Id : Identifiable.S

module On_disk : sig
  type t

  include Sexpable.S with type t := t
end

type 'w t

val create :
    id : ('a -> 'w -> Id.t)
 -> to_drawing : ('a -> 'w -> Drawing.t)
 -> on_step : ('a -> 'w -> ('a * 'w))
 -> on_event : ('a -> 'w -> Sdlevent.t -> ('a * 'w))
 -> state : 'a
 -> 'w t

val id : 'w t -> 'w -> Id.t

val to_drawing : 'w t -> 'w -> Drawing.t

val on_step : 'w t -> 'w -> ('w t * 'w)

val on_event : 'w t -> 'w -> Sdlevent.t -> ('w t * 'w)
