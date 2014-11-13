open Core.Std

module Id = String_id.Make(struct let module_name = "Res.Id" end)

module Mesh = struct
  type t = {
    id          : Id.t;
    file        : string;
    geometry_id : string;
  } with fields, sexp
end

type t =
  | Mesh of Mesh.t
with sexp
