open Async.Std

type 'a resp = [`Continue of 'a | `Quit]

val main_loop :
     initial_state : 'a
  -> on_event : (state : 'a -> Sdlevent.t -> 'a resp)
  -> on_step : ('a -> 'a resp)
  -> steps_per_sec : float
  -> drawing_of_state : ('a -> Drawing.t)
  -> ctx : Drawing.Context.t
  -> unit Deferred.t

val with_sdl :
     f : (   ctx : Drawing.Context.t
          -> width : int
          -> height : int
          -> unit Deferred.t )
  -> data_dir : string
  -> unit Deferred.t
