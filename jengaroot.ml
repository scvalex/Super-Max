open Core.Std let _ = _squelch_unused_module_warning_
open Async.Std
open Jenga_lib.Api

let ( *>>= ) = Dep.bind;;

let ( *>>| ) = Dep.map;;

let message str =
  print_endline ("!!jengaroot.ml: " ^ str)
;;
let _ = message;;

let (^/) dir name = Path.relative ~dir name;;

let (/^) = (^/);;

let basename = Path.basename;;

module Liblinks = struct
  let lib_dir_path ~lib =
    Path.the_root /^ "liblinks" /^ lib
  ;;

  let deps ~suffixes libs =
    Dep.all_unit
      (List.map libs ~f:(fun lib ->
         let dir = lib_dir_path ~lib in
         Dep.all_unit
           (List.map suffixes ~f:(fun suffix ->
              Dep.path (dir ^/ lib ^ suffix)))))
  ;;

  let rules ~lib =
    let liblinks_dir = lib_dir_path ~lib in
    let lib_dir =
      match String.chop_suffix lib ~suffix:"_lib" with
      | None     -> failwith ("Lib name does not end in _lib: " ^ lib)
      | Some lib -> Path.the_root /^ "lib" /^ lib
    in
    List.map [".cmx"; ".cmi"; ".cmxa"; ".a"; ".o"] ~f:(fun suffix ->
      let link_file =
        let source = lib_dir ^/ lib ^ suffix in
        Dep.path source
        *>>| fun () ->
        let relative_source =
          Path.reach_from ~dir:liblinks_dir source
        in
        Action.shell ~dir:liblinks_dir
          ~prog:"ln" ~args:["-sf"; relative_source; "."]
      in
      Rule.create ~targets:[liblinks_dir ^/ lib ^ suffix] link_file)
  ;;

  let include_dirs ~dir libs =
    List.map libs ~f:(fun lib ->
      Path.reach_from ~dir (lib_dir_path ~lib))
  ;;
end

let ocamlopt ~dir ~external_libraries ~for_pack ~include_dirs ~args =
  let packages =
    match external_libraries with
    | [] -> []
    | _  -> ["-package"; String.concat ~sep:"," external_libraries]
  in
  let packages_args = "-thread" :: packages in
  let pack_args =
    Option.value_map for_pack ~default:[]
      ~f:(fun pack -> ["-for-pack"; String.capitalize pack])
  in
  let include_args =
    List.concat_map include_dirs ~f:(fun include_dir ->
      ["-I"; include_dir])
  in
  Action.shell ~dir ~prog:"ocamlfind"
    ~args:(List.concat [["ocamlopt"]; args; packages_args; pack_args; include_args])
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

  let load ~dir =
    Dep.contents (dir ^/ "smbuild")
    *>>| fun smbuild ->
    t_of_sexp (Sexp.of_string (String.strip smbuild))
  ;;
end

let ocamldep_deps ~dir ~source =
  Dep.action_stdout
    (Dep.all_unit
       [ Dep.glob_change (glob_ml ~dir)
       ; Dep.glob_change (glob_mli ~dir)
       ; Dep.path source
       ]
     *>>| fun () ->
     ocamldep ~dir ~args:["-native"; basename source])
  *>>| fun deps ->
  List.map (split_into_lines deps) ~f:(fun dep ->
    match String.split dep ~on:':' with
    | [before; after] ->
      let before =
        Option.value_exn (List.hd (split_into_words before))
          ~message:("invalid ocamldep before: " ^ before)
      in
      (before, split_into_words after)
    | _ ->
      failwith ("invalid ocamldep output line: " ^ dep))
;;

let deps_paths ~dir ~source ~target =
  ocamldep_deps ~dir ~source
  *>>= fun deps ->
  let dep_paths =
    List.map deps ~f:(fun (before, after) ->
      (dir ^/ before, List.map after ~f:(fun after -> dir ^/ after)))
  in
  match List.Assoc.find dep_paths target with
  | None ->
    failwith ("missing in ocamldep output: " ^ Path.to_string target)
  | Some paths ->
    Dep.all_unit
      (List.map paths ~f:(fun path -> Dep.path path))
;;

let compiled_files_for ~dir names =
  List.concat_map names ~f:(fun name ->
    [ Dep.path (dir ^/ name ^ ".cmx")
    ; Dep.path (dir ^/ name ^ ".cmi")
    ; Dep.path (dir ^/ name ^ ".o")
    ])
;;

let link_exe_rule ~dir ~libraries ~external_libraries ~exe names =
  let include_dirs = Liblinks.include_dirs ~dir libraries in
  let link_exe =
    Dep.all_unit (compiled_files_for ~dir names)
    *>>= fun () ->
    Liblinks.deps libraries ~suffixes:[".cmxa"; ".a"]
    *>>| fun () ->
    ocamlopt ~dir ~external_libraries ~for_pack:None ~include_dirs
      ~args:(List.concat
               [ [ "-linkpkg"
                 ; "-o"; basename exe
                 ]
               ; List.map libraries ~f:(fun lib -> lib ^ ".cmxa")
               (* CR ascvortov: Order cmx files topologically based on ocamldep. *)
               ; List.map names ~f:(fun name -> name ^ ".cmx")
               ])
  in
  Rule.create ~targets:[exe] link_exe
;;

let link_lib_rules ~dir ~external_libraries ~lib_cmxa ~lib names =
  let lib_a = dir ^/ lib ^ ".a" in
  let lib_o = dir ^/ lib ^ ".o" in
  let lib_cmx = dir ^/ lib ^ ".cmx" in
  let lib_cmi = dir ^/ lib ^ ".cmi" in
  let link_cmxa =
    Dep.path lib_cmx
    *>>| fun () ->
    ocamlopt ~dir ~external_libraries ~for_pack:None ~include_dirs:[]
      ~args:["-a"; "-o"; basename lib_cmxa; basename lib_cmx]
  in
  let pack_lib_cmx =
    Dep.all_unit (compiled_files_for ~dir names)
    *>>| fun () ->
    ocamlopt ~dir ~external_libraries ~for_pack:None ~include_dirs:[]
      ~args:(List.concat
               [ [ "-pack"
                 ; "-o"; basename lib_cmx
                 ]
               ; List.map names ~f:(fun name -> name ^ ".cmx")
               ])
  in
  [ Rule.create ~targets:[lib_cmxa; lib_a] link_cmxa
  ; Rule.create ~targets:[lib_cmx; lib_cmi; lib_o] pack_lib_cmx
  ]
;;

let compile_ml ~dir ~name ~external_libraries ~libraries ~for_pack ~include_dirs ~cmx =
  let ml = dir ^/ name ^ ".ml" in
  Dep.path ml
  *>>= fun () ->
  Liblinks.deps libraries ~suffixes:[".cmi"; ".cmx"; ".o"]
  *>>= fun () ->
  deps_paths ~dir ~source:ml ~target:cmx
  *>>| fun () ->
  ocamlopt ~dir ~external_libraries ~include_dirs ~for_pack
    ~args:["-c"; basename ml]
;;

let compile_ml_rule ~dir ~libraries ~external_libraries ~for_pack name =
  let cmi = dir ^/ name ^ ".cmi" in
  let cmx = dir ^/ name ^ ".cmx" in
  let o = dir ^/ name ^ ".o" in
  let include_dirs = Liblinks.include_dirs ~dir libraries in
  Rule.create ~targets:[cmi; cmx; o]
    (compile_ml ~dir ~name ~external_libraries ~libraries ~for_pack ~include_dirs ~cmx)
;;

let compile_ml_mli_rules ~dir ~libraries ~external_libraries ~for_pack name =
  let cmi = dir ^/ name ^ ".cmi" in
  let cmx = dir ^/ name ^ ".cmx" in
  let o = dir ^/ name ^ ".o" in
  let include_dirs = Liblinks.include_dirs ~dir libraries in
  let compile_mli =
    let mli = dir ^/ name ^ ".mli" in
    Dep.path mli
    *>>= fun () ->
    Liblinks.deps libraries ~suffixes:[".cmi"]
    *>>= fun () ->
    deps_paths ~dir ~source:mli ~target:cmi
    *>>| fun () ->
    ocamlopt ~dir ~external_libraries ~for_pack ~include_dirs
      ~args:["-c"; basename mli]
  in
  [ Rule.create ~targets:[cmx; o]
      (compile_ml ~dir ~name ~external_libraries ~libraries ~for_pack ~include_dirs ~cmx)
  ; Rule.create ~targets:[cmi] compile_mli
  ]
;;

let compile_mls_in_dir_rules ~dir ~libraries ~external_libraries ~for_pack =
  Dep.glob_listing (glob_ml ~dir)
  *>>= fun mls ->
  Dep.glob_listing (glob_mli ~dir)
  *>>| fun mlis ->
  let names =
    List.map mls ~f:(fun ml ->
      String.chop_suffix_exn (basename ml) ~suffix:".ml")
  in
  let rules =
    List.concat_map names ~f:(fun name ->
      if List.mem mlis (dir ^/ name ^ ".mli")
      then compile_ml_mli_rules ~dir ~libraries ~external_libraries ~for_pack name
      else [compile_ml_rule ~dir ~libraries ~external_libraries ~for_pack name])
  in
  (names, rules)
;;

let app_rules ~dir =
  Smbuild.load ~dir
  *>>= fun smbuild ->
  let external_libraries = Smbuild.external_libraries smbuild in
  let libraries = Smbuild.libraries smbuild in
  let exe = dir ^/ (basename dir) ^ ".exe" in
  compile_mls_in_dir_rules ~dir ~libraries ~external_libraries ~for_pack:None
  *>>| fun (names, compile_mls_rules) ->
  List.concat
    [ [ Rule.default ~dir [Dep.path exe]
      ; link_exe_rule ~dir ~libraries ~external_libraries ~exe names
      ]
    ; compile_mls_rules
    ]
;;

let lib_rules ~dir =
  Smbuild.load ~dir
  *>>= fun smbuild ->
  let external_libraries = Smbuild.external_libraries smbuild in
  let libraries = Smbuild.libraries smbuild in
  let lib = basename dir ^ "_lib" in
  let lib_cmxa = dir ^/ lib ^ ".cmxa" in
  compile_mls_in_dir_rules ~dir ~libraries ~external_libraries ~for_pack:(Some lib)
  *>>| fun (names, compile_mls_rules) ->
  List.concat
    [ [ Rule.default ~dir [Dep.path lib_cmxa] ]
    ; link_lib_rules ~dir ~external_libraries ~lib_cmxa ~lib names
    ; compile_mls_rules
    ]
;;

let liblinks_rules ~dir =
  Dep.return (Liblinks.rules ~lib:(basename dir))
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
      | "app"      -> app_rules ~dir
      | "lib"      -> lib_rules ~dir
      | "liblinks" -> liblinks_rules ~dir
      | _          -> nothing_to_build_rules ~dir
  in
  Scheme.exclude is_smbuild
    (Scheme.rules_dep rules)
;;

let setup () =
  return (Env.create (fun ~dir -> scheme ~dir))
;;
