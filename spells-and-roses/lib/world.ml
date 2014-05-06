open Core.Std

module On_disk = struct
  module V1 = struct
    type t = {
      layers : Entity.On_disk.t Entity.Id.Map.t list;
    } with sexp
  end

  type t = [`V1 of V1.t] with sexp
end

type t = {
  layers : Entity.t Entity.Id.Map.t list;
}
