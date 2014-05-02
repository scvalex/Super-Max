(* open Core.Std *)

module Moving_rectangle : sig
  val run : unit -> unit
end

module Rectangles : sig
  val run : unit -> unit
end

module Static_text : sig
  val run :
       ?data_dir : string
    -> unit
    -> unit
end

module Dancing_banana : sig
  val run : unit -> unit
end
