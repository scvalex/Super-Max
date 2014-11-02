open Core.Std let _ = _squelch_unused_module_warning_
open Async.Std
open Jenga_lib.Api

(* let ( *>>= ) = Dep.bind;; *)
let ( *>>| ) = Dep.map;;

let message str =
  print_endline ("!!jengaroot.ml: " ^ str)
;;

let (^/) dir name =
  Path.relative ~dir name
;;

(* let bash ~dir command_string = *)
(*   Action.shell ~dir ~prog:"bash" *)
(*     ~args:[ "-e"; "-u"; "-o"; "pipefail" *)
(*           ; "-c"; command_string *)
(*           ] *)
(* ;; *)

(* let words_of_string string = *)
(*   let string = String.tr string ~target:'\n' ~replacement:' ' in *)
(*   let words = String.split string ~on:' ' in *)
(*   let words = List.filter words ~f:(function | "" -> false | _ -> true) in *)
(*   words *)
(* ;; *)

(* let file_words path = *)
(*   Dep.contents path *)
(*   *>>| words_of_string *)
(* ;; *)

(* let link_quietly = Path.root_relative "jenga_bin/link-quietly";; *)
(* let ocamlwrapper = Path.root_relative "jenga_bin/ocamlwrapper";; *)
(* let the_root_lib_dir = Path.root_relative "jenga_lib";; *)
(* let ocamlopt_prog = "ocamlopt.opt";; *)

(* module Alias = struct *)
(*   include Alias *)

(*   let default ~dir = *)
(*     Alias.create ~dir "DEFAULT" *)
(*   ;; *)

(*   let recursives = *)
(*     [ default; ] *)
(*   ;; *)
(* end *)

module Smbuild = struct
  module Executable = struct
    type t = {
      libraries : string list;
    } with fields, sexp
  end

  type t =
    | Executable of Executable.t
  with sexp
end

(* module DC = struct *)
(*   type t = { *)
(*     dir : Path.t; *)
(*   } with fields *)

(*   let create = Fields.create;; *)
(* end *)

(* let fake_libname_of_exe exe = *)
(*   "bin__" ^ Smbuild.Executable.name exe *)
(* ;; *)

(* let do_nothing_scheme ~dir = *)
(*   Scheme.rules [Rule.default ~dir []] *)
(* ;; *)

(* let gen_recursive_aliases ~dir = *)
(*   Dep.subdirs ~dir *)
(*   *>>| fun subdirs -> *)
(*   List.map Alias.recursives ~f:(fun make_alias -> *)
(*     Rule.alias (make_alias ~dir) *)
(*       (List.map subdirs ~f:(fun subdir -> *)
(*          Dep.alias (make_alias ~dir:subdir)))) *)
(* ;; *)

(* let create_directory_context ~dir _smbuild = *)
(*   let dc = *)
(*     DC.create ~dir *)
(*   in *)
(*   Dep.return dc *)
(* ;; *)

(* let gen_ocaml_build_rules ~dir _dc smbuild = *)
(*   let module Exe = Smbuild.Executable in *)
(*   match smbuild with *)
(*   | Smbuild.Executable exe -> *)
(*     let exe_path = Path.relative ~dir (Exe.name exe) in *)
(*     let exe_action = *)
(*       let ocamlflags = *)
(*         let disabled_warnings = [3; 4; 29; 40; 41; 42; 44; 45; 48] in *)
(*         let ocamlwarnings = *)
(*           "@a" ^ String.concat (List.map disabled_warnings *)
(*                                   ~f:(fun n -> "-" ^ Int.to_string n)) *)
(*         in *)
(*         [ "-I"; "+camlp4" *)
(*         ; "-w"; ocamlwarnings *)
(*         ; "-strict-sequence"; "-short-paths" *)
(*         ; "-thread"; "-bin-annot" *)
(*         ] *)
(*       in *)
(*       let ocamloptflags = *)
(*         ["-inline"; "20"; "-nodynlink"; "-g"] *)
(*       in *)
(*       let cmxa_for_packs = *)
(*         List.map ~f:(fun name -> name ^ ".cmxa") *)
(*           ["nums"; "unix"; "threads"; "bigarray"; "str"] *)
(*       in *)
(*       file_words (Path.relative ~dir *)
(*                     (fake_libname_of_exe exe ^ ".libdeps")) *)
(*       *>>= fun libs -> *)
(*       let liblink_dir ~lib = *)
(*         Path.relative ~dir:the_root_lib_dir lib *)
(*       in *)
(*       let lib_cmxas = *)
(*         List.concat_map libs ~f:(fun lib -> [ *)
(*             "-I"; Path.dotdot ~dir (liblink_dir ~lib); *)
(*             lib ^ ".cmxa"; *)
(*           ]) *)
(*       in *)
(*       file_words (Path.relative ~dir (Exe.name exe ^ ".objdeps")) *)
(*       *>>| fun objs -> *)
(*       let sub_cmxs_in_correct_order = *)
(*         List.map objs ~f:(fun name -> name ^ ".cmx") *)
(*       in *)
(*       bash ~dir *)
(*         (String.concat ~sep:" " (List.concat [ *)
(*            [ Path.dotdot ~dir link_quietly *)
(*            ; Path.dotdot ~dir ocamlwrapper *)
(*            ; ocamlopt_prog ]; *)
(*            ocamlflags; ocamloptflags; *)
(*            ["-cc"; "g++"]; *)
(*            cmxa_for_packs; *)
(*            lib_cmxas; *)
(*            sub_cmxs_in_correct_order; *)
(*            [ Path.basename (Path.relative ~dir (Exe.name exe ^ ".cmx")) *)
(*            ; "-o"; Path.basename exe_path ]; *)
(*          ])) *)
(*     in *)
(*     let default_rule = Rule.default ~dir [Dep.path exe_path] in *)
(*     let exe_rule = Rule.create ~targets:[exe_path] exe_action in *)
(*     [ default_rule *)
(*     ; exe_rule *)
(*     ] *)
(* ;; *)

(* let gen_libdeps dc ~dir ~libs name = *)
(*   [ *)
(*     gen_interface_deps_from_objinfo dc ~dir ~libname:name; *)

(*     gen_transitive_deps ~dir *)
(*       ~one_step:(return libs) *)
(*       ~dep_wrt_dir:the_root_lib_dir *)
(*       ~template:(fun x -> sprintf "%s/%s.interface.deps" x x) *)
(*       ~target:(name ^ ".inferred-1step.deps"); *)

(*     gen_transitive_deps ~dir *)
(*       ~one_step:(file_words (relative ~dir (name ^ ".inferred-1step.deps"))) *)
(*       ~dep_wrt_dir:the_root_lib_dir *)
(*       ~template:(fun x -> sprintf "%s/%s.libdeps" x x) *)
(*       ~target:(name ^ ".libdeps"); *)

(*     Rule.alias (Alias.libdeps ~dir) [ *)
(*       Dep.path (suffixed ~dir name ".libdeps") *)
(*     ]; *)
(*   ] *)
(* ;; *)

(* let gen_dep_rules ~dir dc smbuild = *)
(*   let libnames = *)
(*     match smbuild with *)
(*     | Smbuild.Executable exe -> [fake_libname_of_exe exe] *)
(*   in *)
(*   let libs = *)
(*     match smbuild with *)
(*     | Smbuild.Executable exe -> Smbuild.Executable.libraries exe *)
(*   in *)
(*   List.concat_map libnames ~f:(fun libname -> *)
(*     gen_libdeps dc ~dir ~libs libname) *)
(* ;; *)

(* let ocaml_build_rules ~dir dc smbuild = *)
(*   List.concat *)
(*     [ gen_ocaml_build_rules ~dir dc smbuild *)
(*     ; gen_dep_rules ~dir dc smbuild *)
(*     ] *)
(* ;; *)

(* let build_ocaml_scheme ~dir = *)
(*   let rules = *)
(*     Smbuild.load ~dir *)
(*     *>>= fun smbuild -> *)
(*     gen_recursive_aliases ~dir *)
(*     *>>= fun recursive_aliases -> *)
(*     match smbuild with *)
(*     | None -> *)
(*       Dep.return recursive_aliases *)
(*     | Some smbuild -> *)
(*       create_directory_context ~dir smbuild *)
(*       *>>| fun dc -> *)
(*       recursive_aliases @ ocaml_build_rules ~dir dc smbuild *)
(*   in *)
(*   Scheme.rules_dep rules *)
(* ;; *)

let build_exe ~dir:_ ~app ~smbuild exe =
  Dep.return (Action.save ~target:exe (app ^ (Sexp.to_string_mach (Smbuild.sexp_of_t smbuild))))
;;

let app_scheme ~dir =
  Scheme.contents (dir ^/ "smbuild") (fun smbuild ->
    let smbuild =
      Smbuild.t_of_sexp (Sexp.of_string (String.strip smbuild))
    in
    let app = Path.basename dir in
    let exe = dir ^/ app ^ ".exe" in
    Scheme.rules
      [ Rule.default ~dir [Dep.path exe]
      ; Rule.create ~targets:[exe] (build_exe ~dir ~app ~smbuild exe)
      ])
;;

let lib_scheme ~dir:_ =
  failwith "lib_scheme not implemented"
;;

let nothing_to_build_scheme ~dir =
  Scheme.rules [Rule.default ~dir []]
;;

let all_apps_scheme ~dir =
  let all_apps =
    Dep.subdirs ~dir:(dir ^/ "app")
    *>>| fun apps ->
    [ Rule.default ~dir
        (List.map apps ~f:(fun app ->
           Dep.path (dir ^/ Path.to_string app)))
    ]
  in
  Scheme.rules_dep all_apps
;;

let scheme ~dir =
  message ("Building scheme for " ^ (Path.to_string dir));
  if Path.(the_root = dir)
  then
    all_apps_scheme ~dir
  else
    match Path.(to_string (dirname dir)) with
    | "app" -> app_scheme ~dir
    | "lib" -> lib_scheme ~dir
    | _     -> nothing_to_build_scheme ~dir
;;

let setup () =
  return (Env.create (fun ~dir -> scheme ~dir))
;;
