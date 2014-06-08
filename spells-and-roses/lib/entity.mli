open Core.Std

module Id : Identifiable.S

type ('c, 'ev) t

val create :
    id : Id.t
 -> to_drawing : ('a -> Drawing.t)
 -> on_step : ('a -> 'a)
 -> on_event : ('a -> 'ev -> 'a)
 -> state : 'a
 -> common : ('a -> 'c)
 -> ('c, 'ev) t

val id : (_, _) t -> Id.t

val to_drawing : (_, _) t -> Drawing.t

val on_step : ('a, 'ev) t -> ('a, 'ev) t

val on_event : ('a, 'ev) t -> 'ev -> ('a, 'ev) t

val common : ('c, 'ev) t -> 'c
