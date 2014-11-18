open Core.Std
open Async.Std

module Id = struct
  module T = struct
    type t = {
      pack : string;
      name : string;
    } with compare, sexp, fields

    let hash = Hashtbl.hash;;
  end

  include T
  include Comparable.Make(T)
  include Hashable.Make(T)

  let filename t =
    t.name ^ ".res"
  ;;

  let to_string t =
    t.pack ^ "/" ^ t.name
  ;;
end

type t = {
  packs : string String.Table.t;
  cache : Res.t Id.Table.t;
}

let t =
  let packs = String.Table.create () in
  let cache = Id.Table.create () in
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
    match Hashtbl.find t.packs (Id.pack id) with
    | None ->
      Deferred.return
        (Or_error.errorf "no path for pack %s (of %s)"
           (Id.pack id) (Id.to_string id))
    | Some dir ->
      let open Deferred.Or_error.Monad_infix in
      Res.load (dir ^/ Id.filename id)
      >>| fun res ->
      match cache_until with
      | `Don't_cache ->
        res
      | `End_of_days ->
        Hashtbl.set t.cache ~key:id ~data:res;
        res
;;
