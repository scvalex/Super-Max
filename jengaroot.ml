open Core.Std let _ = _squelch_unused_module_warning_
open Async.Std
open Jenga_lib.Api

let ( *>>= ) = Dep.bind
let ( *>>| ) = Dep.map

let message str =
  print_endline ("!!jengaroot.ml: " ^ str)
;;

module Alias = struct
  include Alias

  let default ~dir =
    Alias.create ~dir "DEFAULT"
  ;;

  let recursives =
    [ default; ]
  ;;
end

module Smbuild = struct
  module Executables = struct
    type t = {
      names     : string list;
      libraries : string list;
    } with sexp
  end

  type t =
    | Executables of Executables.t
  with sexp

  let load ~dir =
    let smbuild = Path.relative ~dir "smbuild" in
    Dep.file_exists smbuild
    *>>= function
    | false ->
      Dep.return None
    | true ->
      message ("Reading " ^ Path.to_string smbuild);
      Dep.contents smbuild
      *>>| fun smbuild_str ->
      let smbuild_str = String.strip smbuild_str in
      Some (t_of_sexp (Sexp.of_string smbuild_str))
  ;;
end

let do_nothing_scheme ~dir =
  Scheme.rules [Rule.default ~dir []]
;;

let gen_recursive_aliases ~dir =
  Dep.subdirs ~dir
  *>>| fun subdirs ->
  List.map Alias.recursives ~f:(fun make_alias ->
    Rule.alias (make_alias ~dir)
      (List.map subdirs ~f:(fun subdir ->
         Dep.alias (make_alias ~dir:subdir))))
;;

let build_ocaml ~dir =
  let build_ocaml_rules =
    Smbuild.load ~dir
    *>>= fun _smbuilds ->
    gen_recursive_aliases ~dir
    *>>| fun recursive_aliases ->
    recursive_aliases @ []
  in
  Scheme.rules_dep build_ocaml_rules
;;

let scheme ~dir =
  Scheme.switch_glob
    ~def:(do_nothing_scheme ~dir)
    [ ("**smbuild", Scheme.no_rules)
    ; ("example/*", build_ocaml ~dir)
    ]
;;

let setup () =
  return (Env.create (fun ~dir -> scheme ~dir))
;;
