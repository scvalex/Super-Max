open Core.Std let _ = _squelch_unused_module_warning_
open Async.Std
open Jenga_lib.Api
open Std_internal

let scheme ~dir =
  let is_smbuild path =
    match basename path with
    | "smbuild" -> true
    | _         -> false
  in
  let rules =
    if Path.(the_root = dir)
    then
      everything_under_rules ~dir ~subdirs:["app"; "lib"]
    else
      match Path.(to_string (dirname dir)) with
      | "app"      -> Ocaml.app_rules ~dir
      | "lib"      -> Ocaml.lib_rules ~dir
      | "liblinks" -> Ocaml.liblinks_rules ~dir
      | _          -> nothing_to_build_rules ~dir
  in
  Scheme.exclude is_smbuild
    (Scheme.rules_dep rules)
;;

let setup () =
  return (Env.create (fun ~dir -> scheme ~dir))
;;
