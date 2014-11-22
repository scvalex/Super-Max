open Core.Std

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

let create = Fields.create;;

let filename t =
  t.name ^ ".res"
;;

let to_string t =
  t.pack ^ "/" ^ t.name
;;

let analyze_filename file =
  let dir = Filename.dirname file in
  let pack = Filename.basename dir in
  let name = Filename.(chop_extension (basename file)) in
  (`Dir dir, `Pack pack, `Name name)
;;
