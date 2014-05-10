open Core.Std

module Zelda = struct
  module Position = struct
    type t = {
      x : float;
      y : float;
      z : float;
    } with sexp
  end
end

module type Zelda = sig
  type engine
  type t

  val entities : t -> (engine Entity.t * Zelda.Position.t) Entity.Id.Map.t
end
