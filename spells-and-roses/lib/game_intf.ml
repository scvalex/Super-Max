open Core.Std let _ = _squelch_unused_module_warning_

module Resp = struct
  type 'a t = [`Continue of 'a | `Quit]
end

module type Pure = sig
  type t

  val create :
       width : int
    -> height : int
    -> t

  val on_event : t -> Sdlevent.t -> t Resp.t

  val on_step : t -> t Resp.t

  val to_drawing : t -> Drawing.t

  val steps_per_sec : float
end
