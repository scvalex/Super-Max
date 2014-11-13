open Core.Std let _ = _squelch_unused_module_warning_
open Jenga_lib.Api
open Std_internal

let res_exe = Path.the_root /^ "app" /^ "res" /^ "res.exe";;

module Res : sig
  module Id : Identifiable

  module Mesh : sig
    type t with sexp

    val file : t -> string

    val geometry_id : t -> string
  end

  type t =
    | Mesh of Mesh.t
  with sexp

  val id : t -> Id.t

  val target : t -> dir : Path.t -> Path.t
end = struct
  module Id = String_id.Make(struct let module_name = "Res.Id" end)

  module Mesh = struct
    type t = {
      id          : Id.t;
      file        : string;
      geometry_id : string;
    } with fields, sexp
  end

  type t =
    | Mesh of Mesh.t
  with sexp

  let id = function
    | Mesh mesh -> Mesh.id mesh
  ;;

  let target t ~dir =
    dir ^/ Id.to_string (id t) ^ ".res"
  ;;
end

module Resbuild : sig
  type t

  val load : dir : Path.t -> t Dep.t

  val exists : dir : Path.t -> bool Dep.t

  val resources : t -> Res.t list
end = struct
  type t = Res.t list with sexp

  let load ~dir =
    Dep.contents (dir ^/ "resbuild")
    *>>| fun resbuild ->
    t_of_sexp (Sexp.of_string (String.strip resbuild))
  ;;

  let exists ~dir =
    Dep.file_exists (dir ^/ "resbuild")
  ;;

  let resources t = t;;
end

let build_res ~dir res =
  Dep.path res_exe
  *>>= fun () ->
  match res with
  | Res.Mesh mesh ->
    Dep.path (dir ^/ Res.Mesh.file mesh)
    *>>| fun () ->
    Action.shell ~dir ~prog:(Path.reach_from ~dir res_exe)
      ~args:[ "extract"; "mesh"
            ; "-source"; Res.Mesh.file mesh
            ; "-target-id"; Res.Id.to_string (Res.id res)
            ; "-geometry-id"; Res.Mesh.geometry_id mesh
            ]
;;

let asset_rules ~dir =
  Resbuild.exists ~dir
  *>>= fun resbuild_present ->
  if resbuild_present
  then begin
    Resbuild.load ~dir
    *>>| fun resbuild ->
    List.concat
      [ [Rule.default ~dir (List.map (Resbuild.resources resbuild) ~f:(fun res ->
         Dep.path (Res.target res ~dir)))]
      ; List.map (Resbuild.resources resbuild) ~f:(fun res ->
        Rule.create ~targets:[Res.target res ~dir] (build_res ~dir res))
      ]
  end else begin
    nothing_to_build_rules ~dir
  end
;;
