open Async.Std

include module type of Game_intf

(** [main_loop] runs the game's event loop.  It tries to run at a
    fixed number of [steps_per_second] regardless of rendering time.
    Since the Async scheduler is running, it is possible to run Async
    jobs in the event handlers, but they are not allowed to block the
    event loop. *)
val main_loop :
     initial_state : 'a
  -> on_event : ('a -> Sdlevent.t -> 'a Resp.t)
  -> on_step : ('a -> 'a Resp.t)
  -> steps_per_second : float
  -> to_drawing : ('a -> Drawing.t)
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
     (module S)
  -> data_dir : string
  -> unit Deferred.t
