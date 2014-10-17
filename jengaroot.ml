open Core.Std let _ = _squelch_unused_module_warning_
open Async.Std
open Jenga_lib.Api

let ( *>>= ) = Dep.bind;;
let ( *>>| ) = Dep.map;;

let message str =
  print_endline ("!!jengaroot.ml: " ^ str)
;;

let bash ~dir command_string =
  Action.shell ~dir ~prog:"bash"
    ~args:[ "-e"; "-u"; "-o"; "pipefail"
          ; "-c"; command_string
          ]
;;

let link_quietly = Path.root_relative "jenga_bin/link-quietly";;
let ocamlwrapper = Path.root_relative "jenga_bin/ocamlwrapper";;
let ocamlopt_prog = "ocamlopt.opt";;

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
  module Executable = struct
    type t = {
      name      : string;
      libraries : string list;
    } with fields, sexp
  end

  type t =
    | Executable of Executable.t
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

module DC = struct
  type t = {
    dir : Path.t;
  } with fields

  let create = Fields.create;;
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

let create_directory_context ~dir _smbuild =
  let dc =
    DC.create ~dir
  in
  Dep.return dc
;;

let gen_ocaml_build_rules ~dir _dc smbuild =
  let module Exe = Smbuild.Executable in
  match smbuild with
  | Smbuild.Executable executable ->
    let exe_path = Path.relative ~dir (Exe.name executable) in
    let default_rule =
      Rule.default ~dir
        [Dep.path exe_path]
    in
    let ocamlflags =
      let disabled_warnings = [3; 4; 29; 40; 41; 42; 44; 45; 48] in
      let ocamlwarnings =
        "@a" ^ String.concat (List.map disabled_warnings
                                ~f:(fun n -> "-" ^ Int.to_string n))
      in
      [ "-I"; "+camlp4"
      ; "-w"; ocamlwarnings
      ; "-strict-sequence"; "-short-paths"
      ; "-thread"; "-bin-annot"
      ]
    in
    let ocamloptflags =
      ["-inline"; "20"; "-nodynlink"; "-g"]
    in
    let cmxa_for_packs =
      List.map ~f:(fun name -> name ^ ".cmxa")
        ["nums"; "unix"; "threads"; "bigarray"; "str"]
    in
    let lib_cmxas =
      List.concat_map libs ~f:(fun lib -> [
          "-I"; dotdot ~dir (LL.liblink_dir ~lib);
          lib ^ ".cmxa";
        ])
    in
    let exe_rule =
      Rule.create
        ~targets:[exe_path]
        (bash ~dir
           (String.concat ~sep:" " (List.concat [
              [ Path.dotdot ~dir link_quietly
              ; Path.dotdot ~dir ocamlwrapper
              ; ocamlopt_prog ];
              ocamlflags; ocamloptflags;
              ["-cc"; "g++"];
              cmxa_for_packs;
              lib_cmxas;
              sub_cmxs_in_correct_order;
              [basename main_cmx; "-o"; basename exe_path];
            ])))
    in
    [ default_rule
    ; exe_rule
    ]
;;

let ocaml_build_rules ~dir dc smbuild =
  List.concat
    [ gen_ocaml_build_rules ~dir dc smbuild
    ]
;;

let build_ocaml_scheme ~dir =
  let rules =
    Smbuild.load ~dir
    *>>= fun smbuild ->
    gen_recursive_aliases ~dir
    *>>= fun recursive_aliases ->
    match smbuild with
    | None ->
      Dep.return recursive_aliases
    | Some smbuild ->
      create_directory_context ~dir smbuild
      *>>| fun dc ->
      recursive_aliases @ ocaml_build_rules ~dir dc smbuild
  in
  Scheme.rules_dep rules
;;

let scheme ~dir =
  Scheme.switch_glob
    ~def:(do_nothing_scheme ~dir)
    [ ("**smbuild", Scheme.no_rules)
    ; ("example/*", build_ocaml_scheme ~dir)
    ]
;;

let setup () =
  return (Env.create (fun ~dir -> scheme ~dir))
;;
