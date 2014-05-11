open Core.Std
open Async.Std
open Ocaml_plugin.Std

module World_plugin = Ocaml_compiler.Make(struct
  type t = (module World_intf.S)
  let t_repr = "World_intf.S";;
  let univ_constr = World_intf.univ_constr;;
  let univ_constr_repr = "World_intf.univ_constr";;
end)

let load ~file =
  World_plugin.load_ocaml_src_files [file]
  |! Deferred.Or_error.ok_exn
;;

let edit ~file =
  load ~file
  >>= fun _world ->
  Deferred.unit
;;
