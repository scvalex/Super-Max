open Core.Std let _ = _squelch_unused_module_warning_
open Async.Std
open Jenga_lib.Api
open Std_internal

let top_level_rules ~dir =
  everything_under_targets ~dir ~subdirs:["app"; "lib"; "assets"]
  *>>| fun sub_targets ->
  let readme = dir ^/ "README.md" in
  let readme_rule =
    let make_readme =
      Dep.List.concat_map ["app"; "lib"] ~f:(fun subdir ->
        Dep.subdirs ~dir:(dir ^/ subdir))
      *>>= fun dirs ->
      Ocaml.External_deps.extract ~dirs
      *>>= fun deps ->
      Dep.contents (dir ^/ "README.md.in")
      *>>| fun text ->
      let format_libs libs =
        String.concat ~sep:", " (List.map libs ~f:(fun lib -> "`" ^ lib ^ "`"))
      in
      let substitute_dep_list = function
        | "dep_list" ->
          String.concat ~sep:"\n"
            [ "  - OCaml libraries: " ^
              format_libs (Ocaml.External_deps.external_libraries deps)
            ; "  - other libraries: " ^
              format_libs (Ocaml.External_deps.foreign_libraries deps)
            ]
        | name ->
          name
      in
      let buf = Bigbuffer.create 1024 in
      Bigbuffer.add_substitute buf substitute_dep_list text;
      Action.save (Bigbuffer.contents buf) ~target:readme
    in
    Rule.create ~targets:[readme] make_readme
  in
  [ readme_rule
  ; Rule.default ~dir (Dep.path readme :: List.map sub_targets ~f:Dep.path)
  ]
;;

let scheme ~dir =
  let is_mlbuild path =
    match basename path with
    | "mlbuild"  -> true
    | "resbuild" -> true
    | _          -> false
  in
  let rules =
    if Path.(the_root = dir)
    then
      top_level_rules ~dir
    else
      match Path.(to_string (dirname dir)) with
      | "app"      -> Ocaml.app_rules ~dir
      | "lib"      -> Ocaml.lib_rules ~dir
      | "liblinks" -> Ocaml.liblinks_rules ~dir
      | "assets"   -> Assets.asset_rules ~dir
      | _          -> nothing_to_build_rules ~dir
  in
  Scheme.exclude is_mlbuild
    (Scheme.rules_dep rules)
;;

let setup () =
  return (Env.create (fun ~dir -> scheme ~dir))
;;
