open Core.Std

module Id : Identifiable.S

type ('a, 'w) t

val create :
    id : Id.t
 -> to_drawing : ('a -> 'w -> Drawing.t)
 -> on_step : ('a -> 'w -> ('a * 'w))
 -> on_event : ('a -> 'w -> Sdlevent.t -> ('a * 'w))
 -> state : 'a
 -> ('a, 'w) t

val id : (_, 'w) t -> Id.t

val to_drawing : (_, 'w) t -> 'w -> Drawing.t

val on_step : ('a, 'w) t -> 'w -> (('a, 'w) t * 'w)

val on_event : ('a, 'w) t -> 'w -> Sdlevent.t -> (('a, 'w) t * 'w)

val state : ('a, 'w) t -> 'a
