(* open Core.Std *)

type 'a resp = [`Continue of 'a | `Quit]

val main_loop :
     initial_state : 'a
  -> on_event : (state : 'a -> Sdlevent.t -> 'a resp)
  -> on_step : ('a -> 'a resp)
  -> steps_per_sec : float
  -> drawing_of_state : ('a -> Drawing.t)
  -> renderer : Sdlrender.t
  -> unit

val with_sdl :
     f : (   renderer : Sdlrender.t
          -> width : int
          -> height : int
          -> unit )
  -> unit
