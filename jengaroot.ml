open Core.Std let _ = _squelch_unused_module_warning_
open Async.Std
open Jenga_lib.Api

let ( *>>= ) = Dep.bind;;

let ( *>>| ) = Dep.map;;

let message str =
  print_endline ("!!jengaroot.ml: " ^ str)
;;
let _ = message;;

let (^/) dir name =
  Path.relative ~dir name
;;

let basename = Path.basename;;

let ocamlopt ~dir ~external_libraries ~args =
  let packages =
    match external_libraries with
    | [] -> []
    | _  -> ["-package"; String.concat ~sep:"," external_libraries]
  in
  let packages = "-thread" :: packages in
  Action.shell ~dir ~prog:"ocamlfind"
    ~args:(List.concat [["ocamlopt"]; args; packages])
;;

let ocamldep ~dir ~args =
  Action.shell ~dir ~prog:"ocamlfind" ~args:("ocamldep" :: args)
;;

let glob_ml ~dir =
  Glob.create ~dir "*.ml"
;;

let glob_mli ~dir =
  Glob.create ~dir "*.mli"
;;

let non_blank str =
  match String.strip str with
  | "" -> false
  | _  -> true
;;

let split_into_lines str =
  List.filter ~f:non_blank (String.split ~on:'\n' str)
;;

let split_into_words str =
  List.filter ~f:non_blank (String.split ~on:'\ ' str)
;;

module Smbuild = struct
  type t = {
    libraries          : string list;
    external_libraries : string list;
  } with fields, sexp
end

let link_exe_rule ~dir ~external_libraries ~exe names =
  let link_exe =
    Dep.all_unit
      (List.concat_map names ~f:(fun name ->
         [ Dep.path (dir ^/ name ^ ".cmx")
         ; Dep.path (dir ^/ name ^ ".o")
         ]))
    *>>| fun () ->
    ocamlopt ~dir ~external_libraries
      ~args:(List.concat
               [ [ "-linkpkg"
                 ; "-o"; basename exe
                 ]
               ; List.map names ~f:(fun name -> name ^ ".cmx")
               ])
  in
  Rule.create ~targets:[exe] link_exe
;;

let ocamldep_deps ~dir ~source ~target =
  Dep.action_stdout
    (Dep.all_unit
       [ Dep.glob_change (glob_ml ~dir)
       ; Dep.glob_change (glob_mli ~dir)
       ; Dep.path source
       ]
     *>>| fun () ->
     ocamldep ~dir ~args:["-native"; basename source])
  *>>= fun deps ->
  let dep_paths =
    List.map (split_into_lines deps) ~f:(fun dep ->
      match String.split dep ~on:':' with
      | [before; after] ->
        let before =
          Option.value_exn (List.hd (split_into_words before))
            ~message:("invalid ocamldep before: " ^ before)
        in
        let after = split_into_words after in
        (dir ^/ before, List.map after ~f:(fun after -> dir ^/ after))
      | _ ->
        failwith ("invalid ocamldep output line: " ^ dep))
  in
  match List.Assoc.find dep_paths target with
  | None ->
    failwith ("missing in ocamldep output: " ^ Path.to_string target)
  | Some paths ->
    Dep.all_unit
      (List.map paths ~f:(fun path -> Dep.path path))
;;

let compile_ml_rule ~dir ~external_libraries name =
  let cmi = dir ^/ name ^ ".cmi" in
  let cmx = dir ^/ name ^ ".cmx" in
  let o = dir ^/ name ^ ".o" in
  let compile_ml =
    let ml = dir ^/ name ^ ".ml" in
    Dep.path ml
    *>>= fun () ->
    ocamldep_deps ~dir ~source:ml ~target:cmx
    *>>| fun () ->
    ocamlopt ~dir ~external_libraries
      ~args:["-c"; basename ml]
  in
  Rule.create ~targets:[cmi; cmx; o] compile_ml
;;

let compile_ml_mli_rules ~dir ~external_libraries name =
  let cmi = dir ^/ name ^ ".cmi" in
  let cmx = dir ^/ name ^ ".cmx" in
  let o = dir ^/ name ^ ".o" in
  let compile_ml =
    let ml = dir ^/ name ^ ".ml" in
    Dep.path ml
    *>>= fun () ->
    ocamldep_deps ~dir ~source:ml ~target:cmx
    *>>| fun () ->
    ocamlopt ~dir ~external_libraries
      ~args:["-c"; basename ml]
  in
  let compile_mli =
    let mli = dir ^/ name ^ ".mli" in
    Dep.path mli
    *>>= fun () ->
    ocamldep_deps ~dir ~source:mli ~target:cmi
    *>>| fun () ->
    ocamlopt ~dir ~external_libraries
      ~args:["-c"; basename mli]
  in
  [ Rule.create ~targets:[cmx; o] compile_ml
  ; Rule.create ~targets:[cmi] compile_mli
  ]
;;

let app_rules ~dir =
  Dep.contents (dir ^/ "smbuild")
  *>>= fun smbuild ->
  let smbuild =
    Smbuild.t_of_sexp (Sexp.of_string (String.strip smbuild))
  in
  let external_libraries =
    Smbuild.external_libraries smbuild
  in
  let app = basename dir in
  let exe = dir ^/ app ^ ".exe" in
  Dep.glob_listing (glob_ml ~dir)
  *>>= fun mls ->
  Dep.glob_listing (glob_mli ~dir)
  *>>| fun mlis ->
  let names =
    List.map mls ~f:(fun ml ->
      String.chop_suffix_exn (basename ml) ~suffix:".ml")
  in
  let compile_ml_rules =
    List.concat_map names ~f:(fun name ->
      if List.mem mlis (dir ^/ name ^ ".mli")
      then compile_ml_mli_rules ~dir ~external_libraries name
      else [compile_ml_rule ~dir ~external_libraries name])
  in
  List.concat
    [ [ Rule.default ~dir [Dep.path exe]
      ; link_exe_rule ~dir ~external_libraries ~exe names
      ]
    ; compile_ml_rules
    ]
;;

let lib_rules ~dir:_ =
  failwith "lib_scheme not implemented"
;;

let nothing_to_build_rules ~dir =
  Dep.return [Rule.default ~dir []]
;;

let everything_under_rules ~dir ~subdirs =
  let subdirs =
    List.map subdirs ~f:(fun subdir ->
      dir ^/ subdir)
  in
  Dep.List.concat_map subdirs ~f:(fun subdir ->
    Dep.subdirs ~dir:subdir)
  *>>| fun targets ->
  [ Rule.default ~dir (List.map targets ~f:Dep.path)
  ]
;;

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
      | "app" -> app_rules ~dir
      | "lib" -> lib_rules ~dir
      | _     -> nothing_to_build_rules ~dir
  in
  Scheme.exclude is_smbuild
    (Scheme.rules_dep rules)
;;

let setup () =
  return (Env.create (fun ~dir -> scheme ~dir))
;;
