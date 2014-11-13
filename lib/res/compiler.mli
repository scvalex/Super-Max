open Core_kernel.Std
open Async_kernel.Std

val compile :
  dir : string
  -> unit Deferred.Or_error.t
