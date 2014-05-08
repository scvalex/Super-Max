open Core.Std
open Async.Std
open Ocaml_plugin.Std

module Script = Ocaml_compiler.Make(struct
  type t = (module Script_intf.S)
  let t_repr = "Script_intf.S";;
  let univ_constr = Script_intf.univ_constr;;
  let univ_constr_repr = "Script_intf.univ_constr";;
end)

let run ~file =
  Script.load_ocaml_src_files [file]
  |! Deferred.Or_error.ok_exn
  >>= fun script ->
  let module M = (val script : Script_intf.S) in
  M.run ()
;;
