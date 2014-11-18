open Core.Std
open Async.Std

type t = {
  packs : string String.Table.t;
  cache : Res.t Res_id.Table.t;
}

let t =
  let packs = String.Table.create () in
  let cache = Res_id.Table.create () in
  { packs; cache; }
;;

let add_pack ~dir =
  let dir = Filename.realpath dir in
  let pack = Filename.dirname dir in
  if Hashtbl.mem t.packs pack then
    failwithf "Already have pack %s at %s" pack dir ();
  Hashtbl.set t.packs ~key:pack ~data:dir
;;

let load ~id ~cache_until =
  match Hashtbl.find t.cache id with
  | Some res ->
    Deferred.Or_error.return res
  | None ->
    match Hashtbl.find t.packs (Res_id.pack id) with
    | None ->
      Deferred.return
        (Or_error.errorf "no path for pack %s (of %s)"
           (Res_id.pack id) (Res_id.to_string id))
    | Some dir ->
      let open Deferred.Or_error.Monad_infix in
      Res.load ~id (dir ^/ Res_id.filename id)
      >>| fun res ->
      match cache_until with
      | `Don't_cache ->
        res
      | `End_of_days ->
        Hashtbl.set t.cache ~key:id ~data:res;
        res
;;
