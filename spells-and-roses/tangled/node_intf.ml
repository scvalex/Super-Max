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

  (** The [State.t] of a [Node.t] is a pure representation of a
      snapshot of the game state.  It must be pure because the node
      makes copies of it, applies different events to different
      copies, and sends it over the wire. *)
  type t

  include Binable.S with type t := t
  include Sexpable.S with type t := t

  val on_step : t -> t

  val on_event : t -> Event.t -> t
end
