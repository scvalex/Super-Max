open Linear.Std

type t

val create :
  width : int
  -> height : int
  -> unit
  -> t

val width : t -> int

val height : t -> int

val set :
  t
  -> x : int
  -> y : int
  -> Radiance3.t
  -> unit

val get :
  t
  -> x : int
  -> y : int
  -> Radiance3.t

val save_ppm :
  t
  -> filename : string
  -> ?device_gamma : float
  -> ?display_constant : float
  -> unit
  -> unit

module Test : sig
  val checkerboard : unit -> t
end
