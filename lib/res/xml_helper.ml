open Core.Std
open Async.Std

type t = Xml.xml

let parse_file file =
  Deferred.Or_error.try_with (fun () ->
    In_thread.run (fun () ->
      Xml.parse_file file))
;;

let children t =
  match t with
  | Xml.Element (_, _, children) -> children
  | Xml.PCData _                 -> []
;;

let tag t =
  match t with
  | Xml.Element (tag, _, _) -> Some tag
  | Xml.PCData _            -> None
;;

let attr t key =
  let key = String.lowercase key in
  match t with
  | Xml.Element (_, attrs, _) ->
    List.find_map attrs ~f:(fun (key', value) ->
      let key' = String.lowercase key' in
      if String.(key = key')
      then Some value
      else None)
  | Xml.PCData _ ->
    None
;;

module Query = struct
  type one = (t list -> t list)
  type t = one list

  let empty = [];;

  let (+>) t q =
    t @ [q]
  ;;

  let tag tag1 xmls =
    let xmls = List.concat_map xmls ~f:children in
    List.filter xmls ~f:(fun xml ->
      match tag xml with
      | None      -> false
      | Some tag2 -> String.(tag1 = tag2))
  ;;

  let attr key value xmls =
    let xmls = List.concat_map xmls ~f:children in
    List.filter xmls ~f:(fun xml ->
      match attr xml key with
      | None        -> false
      | Some value' -> String.(value = value'))
  ;;

  let run t xml =
    List.fold_left t ~init:[xml] ~f:(fun xmls q ->
      q xmls)
  ;;
end

let query t query =
  Query.run query t
;;
