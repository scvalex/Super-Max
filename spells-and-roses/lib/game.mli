open Async.Std

include module type of Game_intf

val main_loop :
     initial_state : 'a
  -> on_event : ('a -> Sdlevent.t -> 'a Resp.t)
  -> on_step : ('a -> 'a Resp.t)
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

val run :
     (module Game_intf.S)
  -> data_dir : string
  -> unit Deferred.t
