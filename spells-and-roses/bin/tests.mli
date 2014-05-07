open Core.Std
open Async.Std

module Moving_rectangle : sig
  val run :
       data_dir : String.t
    -> unit Deferred.t
end

module Rectangles : sig
  val run :
       data_dir : String.t
    -> unit Deferred.t
end

module Static_text : sig
  val run :
       data_dir : String.t
    -> unit Deferred.t
end

module Dancing_banana : sig
  val run :
       data_dir : String.t
    -> unit Deferred.t
end

module Psy_cat : sig
  val run :
       data_dir : String.t
    -> unit Deferred.t
end
