open Core.Std let _ = _squelch_unused_module_warning_
open Async.Std
open Jenga_lib.Api
open Std_internal

module Lib : sig
  type t

  include Comparable.S with type t := t
  include Hashable.S with type t := t

  val name : t -> string
  val of_name : string -> t

  val suffixed_name : t -> string
  val of_suffixed_name : string -> t

  val dir : t -> Path.t
end = struct
  include String_id.Make(struct let module_name = "Jengaroot.Lib" end)

  let name = to_string;;

  let of_name str =
    if String.is_suffix str ~suffix:"_lib" then
      failwith ("Lib name already suffixed: " ^ str);
    of_string str
  ;;

  let suffixed_name t =
    to_string t ^ "_lib"
  ;;

  let of_suffixed_name str =
    match String.chop_suffix str ~suffix:"_lib" with
    | None      -> failwith ("Suffixed lib name missing suffix: " ^ str)
    | Some name -> of_name name
  ;;

  let dir t =
    Path.the_root /^ "lib" /^ to_string t
  ;;
end

module Liblinks = struct
  let lib_dir_path ~lib =
    Path.the_root /^ "liblinks" /^ Lib.suffixed_name lib
  ;;

  let deps ~suffixes libs =
    Dep.all_unit
      (List.map libs ~f:(fun lib ->
         let dir = lib_dir_path ~lib in
         Dep.all_unit
           (List.map suffixes ~f:(fun suffix ->
              Dep.path (dir ^/ Lib.suffixed_name lib ^ suffix)))))
  ;;

  let rules ~lib =
    let liblinks_dir = lib_dir_path ~lib in
    List.map [".cmx"; ".cmi"; ".cmxa"; ".a"; ".o"] ~f:(fun suffix ->
      let file = (Lib.suffixed_name lib) ^ suffix in
      let link_file =
        let source = Lib.dir lib ^/ file in
        Dep.path source
        *>>| fun () ->
        let relative_source =
          Path.reach_from ~dir:liblinks_dir source
        in
        Action.shell ~dir:liblinks_dir
          ~prog:"ln" ~args:["-sf"; relative_source; "."]
      in
      Rule.create ~targets:[liblinks_dir ^/ file] link_file)
  ;;

  let include_dirs ~dir libs =
    List.map libs ~f:(fun lib ->
      Path.reach_from ~dir (lib_dir_path ~lib))
  ;;
end

let pp_packages =
  [ "camlp4"; "sexplib.syntax"; "bin_prot.syntax"; "herelib.syntax"
  ; "fieldslib.syntax"; "comparelib.syntax"
  ]
;;

let ocamlopt ~dir ~external_libraries ~foreign_libraries ~for_pack
      ~include_dirs ~args ~allow_unused_opens =
  let packages =
    match external_libraries with
    | [] -> []
    | _  -> ["-package"; String.concat ~sep:"," (pp_packages @ external_libraries)]
  in
  let packages_args = "-thread" :: packages in
  let pack_args =
    Option.value_map for_pack ~default:[]
      ~f:(fun pack -> ["-for-pack"; String.capitalize (Lib.suffixed_name pack)])
  in
  let include_args =
    List.concat_map include_dirs ~f:(fun include_dir ->
      ["-I"; include_dir])
  in
  let foreign_args =
    match foreign_libraries with
    | [] ->
      []
    | _ ->
      ["-ccopt"; "-L/usr/lib64"]
      @ List.concat_map foreign_libraries ~f:(fun foreign_library ->
        ["-cclib"; "-l" ^ foreign_library])
  in
  let warning_args =
    let ignored_warnings = ["4"; "23"; "41"; "44"; "45"] in
    let ignored_warnings =
      if allow_unused_opens then "33" :: ignored_warnings else ignored_warnings
    in
    ["-w"; String.concat ~sep:"-" ("@A" :: ignored_warnings)]
  in
  let syntax_args =
    ["-syntax"; "camlp4o"]
  in
  Action.shell ~dir ~prog:"ocamlfind"
    ~args:(List.concat [ ["ocamlopt"]; args; packages_args; pack_args; include_args
                       ; foreign_args; warning_args; syntax_args
                       ])
;;

let ocamldep ~dir ~args =
  let packages_args =
    ["-package"; String.concat ~sep:"," pp_packages]
  in
  let syntax_args =
    ["-syntax"; "camlp4o"]
  in
  Action.shell ~dir ~prog:"ocamlfind"
    ~args:(List.concat [ ["ocamldep"]; args; packages_args; syntax_args
                       ])
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

module Smbuild : sig
  type t

  val load : dir : Path.t -> t Dep.t

  val exists : dir : Path.t -> bool Dep.t

  val libraries : t -> Lib.t list

  val external_libraries : t -> string list

  val foreign_libraries : t -> string list
end = struct
  type t = {
    libraries          : string list sexp_option;
    external_libraries : string list sexp_option;
    foreign_libraries  : string list sexp_option;
  } with sexp

  let load ~dir =
    Dep.contents (dir ^/ "smbuild")
    *>>| fun smbuild ->
    t_of_sexp (Sexp.of_string (String.strip smbuild))
  ;;

  let exists ~dir =
    Dep.file_exists (dir ^/ "smbuild")
  ;;

  let libraries t =
    List.map (Option.value ~default:[] t.libraries) ~f:Lib.of_name
  ;;

  let external_libraries t =
    Option.value ~default:[] t.external_libraries
  ;;

  let foreign_libraries t =
    Option.value ~default:[] t.foreign_libraries
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

let toposort ~targets deps =
  let sorted_targets = Queue.create () in
  let rec loop deps =
    if Hashtbl.is_empty deps
    then begin
      ()
    end else begin
      let (ready, still_have_deps) =
        Hashtbl.partition_map deps ~f:(fun target_deps ->
          if List.is_empty target_deps
          then `Fst ()
          else `Snd target_deps)
      in
      if Hashtbl.is_empty ready then
        failwith ("Toposort failed on: "
                  ^ (String.concat ~sep:" " (Set.to_list targets)));
      let ready = String.Set.of_list (Hashtbl.keys ready) in
      Set.iter ready ~f:(Queue.enqueue sorted_targets);
      let still_have_deps =
        String.Table.map still_have_deps ~f:(fun target_deps ->
          List.filter target_deps ~f:(fun target_dep ->
            not (Set.mem ready target_dep)))
      in
      loop still_have_deps
    end
  in
  loop deps;
  Queue.to_list sorted_targets
;;

let toposort_deps ~dir targets =
  Dep.glob_listing (glob_ml ~dir)
  *>>= fun mls ->
  Dep.all (List.map mls ~f:(fun ml -> ocamldep_deps ~dir ~source:ml))
  *>>| fun deps ->
  let targets = String.Set.of_list targets in
  let deps =
    String.Table.of_alist_exn
      (List.filter_map (List.concat deps) ~f:(fun (target, target_deps) ->
         if Set.mem targets target
         then Some (target, List.filter target_deps ~f:(Set.mem targets))
         else None))
  in
  toposort ~targets deps
;;

let transitive_libraries ~libraries =
  let rec loop ~found lib =
    Smbuild.load ~dir:(Lib.dir lib)
    *>>= fun smbuild ->
    let libraries = Smbuild.libraries smbuild in
    if List.exists libraries ~f:(List.mem found) then
      failwith ("Dependency cycle in library deps for " ^ Lib.name lib);
    Dep.all (List.map libraries ~f:(loop ~found:(lib :: found)))
    *>>| fun lib_deps ->
    (lib :: List.concat lib_deps)
  in
  Dep.List.concat_map libraries ~f:(loop ~found:[])
  *>>| fun libs ->
  Lib.Set.(to_list (of_list libs))
;;

let library_deps ~libraries =
  Dep.all (List.map libraries ~f:(fun lib ->
    Smbuild.load ~dir:(Lib.dir lib)
    *>>| fun smbuild ->
    (lib, smbuild)))
  *>>| fun smbuilds ->
  let (deps, external_libraries) =
    List.fold_left ~init:([], []) smbuilds
      ~f:(fun (deps, ext) (lib, smbuild) ->
        ((Lib.name lib, List.map ~f:Lib.name (Smbuild.libraries smbuild)) :: deps,
         ext @ Smbuild.external_libraries smbuild))
  in
  (String.Table.of_alist_exn deps,
   `External external_libraries)
;;

let link_exe_rule ~dir ~libraries ~external_libraries ~foreign_libraries ~exe names =
  let link_exe =
    Dep.all_unit (compiled_files_for ~dir names)
    *>>= fun () ->
    transitive_libraries ~libraries
    *>>= fun libraries ->
    library_deps ~libraries
    *>>= fun (lib_deps, `External lib_external_libraries) ->
    let external_libraries =
      String.Set.(to_list (of_list (external_libraries @ lib_external_libraries)))
    in
    let libraries =
      let targets = String.Set.of_list (Hashtbl.keys lib_deps) in
      List.map ~f:Lib.of_name (toposort ~targets lib_deps)
    in
    Liblinks.deps libraries ~suffixes:[".cmxa"; ".a"]
    *>>= fun () ->
    toposort_deps ~dir (List.map names ~f:(fun name -> name ^ ".cmx"))
    *>>| fun cmxs ->
    ocamlopt ~dir ~external_libraries ~foreign_libraries
      ~for_pack:None ~allow_unused_opens:false
      ~include_dirs:(Liblinks.include_dirs ~dir libraries)
      ~args:(List.concat
               [ [ "-linkpkg"
                 ; "-o"; basename exe
                 ]
               ; List.map libraries ~f:(fun lib -> Lib.suffixed_name lib ^ ".cmxa")
               ; cmxs
               ])
  in
  Rule.create ~targets:[exe] link_exe
;;

let link_lib_rules ~dir ~external_libraries ~foreign_libraries ~lib_cmxa ~lib names =
  let lib_a = dir ^/ Lib.suffixed_name lib ^ ".a" in
  let lib_o = dir ^/ Lib.suffixed_name lib ^ ".o" in
  let lib_cmx = dir ^/ Lib.suffixed_name lib ^ ".cmx" in
  let lib_cmi = dir ^/ Lib.suffixed_name lib ^ ".cmi" in
  let link_cmxa =
    Dep.all_unit (List.map ~f:Dep.path [lib_cmx; lib_cmi; lib_o])
    *>>| fun () ->
    ocamlopt ~dir ~external_libraries ~foreign_libraries
      ~include_dirs:[] ~for_pack:None ~allow_unused_opens:false
      ~args:["-a"; "-o"; basename lib_cmxa; basename lib_cmx]
  in
  let pack_lib_cmx =
    Dep.all_unit (compiled_files_for ~dir names)
    *>>| fun () ->
    ocamlopt ~dir ~external_libraries ~allow_unused_opens:false
      ~for_pack:None ~include_dirs:[] ~foreign_libraries:[]
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
    ~foreign_libraries:[] ~allow_unused_opens:false
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
      ~foreign_libraries:[] ~allow_unused_opens:true
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

let nothing_to_build_rules ~dir =
  Dep.return [Rule.default ~dir []]
;;

let app_rules ~dir =
  Smbuild.exists ~dir
  *>>= fun smbuild_present ->
  if smbuild_present
  then begin
    Smbuild.load ~dir
    *>>= fun smbuild ->
    let external_libraries = Smbuild.external_libraries smbuild in
    let foreign_libraries = Smbuild.foreign_libraries smbuild in
    let libraries = Smbuild.libraries smbuild in
    let exe = dir ^/ (basename dir) ^ ".exe" in
    compile_mls_in_dir_rules ~dir ~libraries ~external_libraries ~for_pack:None
    *>>| fun (names, compile_mls_rules) ->
    List.concat
      [ [ Rule.default ~dir [Dep.path exe]
        ; link_exe_rule ~dir ~libraries ~external_libraries ~foreign_libraries ~exe names
        ]
      ; compile_mls_rules
      ]
  end else begin
    nothing_to_build_rules ~dir
  end
;;

let lib_rules ~dir =
  Smbuild.exists ~dir
  *>>= fun smbuild_present ->
  if smbuild_present
  then begin
    Smbuild.load ~dir
    *>>= fun smbuild ->
    let external_libraries = Smbuild.external_libraries smbuild in
    let foreign_libraries = Smbuild.foreign_libraries smbuild in
    let libraries = Smbuild.libraries smbuild in
    let lib = Lib.of_name (basename dir) in
    let lib_cmxa = dir ^/ Lib.suffixed_name lib ^ ".cmxa" in
    compile_mls_in_dir_rules ~dir ~libraries ~external_libraries ~for_pack:(Some lib)
    *>>| fun (names, compile_mls_rules) ->
    List.concat
      [ [ Rule.default ~dir [Dep.path lib_cmxa] ]
      ; link_lib_rules ~dir ~external_libraries ~foreign_libraries ~lib_cmxa ~lib names
      ; compile_mls_rules
      ]
  end else begin
    nothing_to_build_rules ~dir
  end
;;

let liblinks_rules ~dir =
  Dep.return (Liblinks.rules ~lib:(Lib.of_suffixed_name (basename dir)))
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
