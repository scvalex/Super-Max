open Async.Std

include module type of Game_intf

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
