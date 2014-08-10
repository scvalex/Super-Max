open Core.Std

module Id = struct
  include String_id
end

module type State = sig
  module Event : sig
    type t

    include Comparable.S with type t := t

    val source : t -> Id.t

    val step : t -> int
  end

  type t

  include Binable.S with type t := t
  include Sexpable.S with type t := t

  val on_step : t -> t

  val on_event : t -> Event.t -> t
end
