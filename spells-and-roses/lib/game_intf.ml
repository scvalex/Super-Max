open Core.Std

module Resp = struct
  type 'a t = [`Continue of 'a | `Quit]
end

module type S = sig
  module Update : sig
    module Query : sig
      type t

      include Binable.S with type t := t
      include Sexpable.S with type t := t
    end

    type t

    include Binable.S with type t := t
    include Sexpable.S with type t := t
  end

  type t

  val create :
       width : int
    -> height : int
    -> t

  val on_event : t -> Sdlevent.t -> t Resp.t

  val on_step : t -> t Resp.t

  val to_drawing : t -> Drawing.t

  val steps_per_second : float
end
