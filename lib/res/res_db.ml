open Core.Std
open Async.Std

(* CR scvalex: Add one level of namespaces. *)

type t = {
  mutable search_paths : string list;
  cache                : Res.t Res.Id.Table.t;
}

let t =
  let search_paths = [] in
  let cache = Res.Id.Table.create () in
  { search_paths; cache; }
;;

let add_search_path ~dir =
  t.search_paths <- dir :: t.search_paths
;;

let load ~id ~cache_until =
  match Hashtbl.find t.cache id with
  | Some res ->
    Deferred.Or_error.return res
  | None ->
    let filename = Res.filename id in
    Deferred.List.find t.search_paths ~f:(fun search_path ->
      Sys.file_exists_exn (search_path ^/ filename))
    >>= function
    | None ->
      Deferred.return (Or_error.errorf "no %s in search paths (%s)"
                         filename (String.concat ~sep:", " t.search_paths))
    | Some search_path ->
      let open Deferred.Or_error.Monad_infix in
      Res.load (search_path ^/ filename)
      >>| fun res ->
      match cache_until with
      | `Don't_cache ->
        res
      | `End_of_days ->
        Hashtbl.set t.cache ~key:id ~data:res;
        res
;;
