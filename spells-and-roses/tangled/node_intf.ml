open Core.Std

module Id = struct
  include String_id
end

module type State = sig
  type t

  val on_step : t -> t

  val on_event : t -> ev -> t
end

module type Event = sig
  type t

  module Comparable.S with type t := t

  val source : t -> Id.t

  val step : t -> int
end
