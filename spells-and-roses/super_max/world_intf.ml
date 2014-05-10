open Core.Std

module Zelda = struct
  module Position = struct
    type t = {
      x : float;
      y : float;
      z : int;
    } with sexp
  end
end

module type Zelda = sig
  type world
  type t

  val entities : t -> (world Entity.t * Zelda.Position.t) Entity.Id.Map.t
end
