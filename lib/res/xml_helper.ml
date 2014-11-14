open Core.Std
open Async.Std

type t = Xml.xml

let parse_file_exn file =
  In_thread.run (fun () ->
    Xml.parse_file file)
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

  let run t xml =
    List.fold_left t ~init:[xml] ~f:(fun xmls one ->
      one xmls)
  ;;

  let empty = [];;

  let (+>) t one =
    t @ [one]
  ;;

  let tag tag1 xmls =
    let tag1 = String.lowercase tag1 in
    let xmls = List.concat_map xmls ~f:children in
    if String.(tag1 = "*")
    then
      xmls
    else
      List.filter xmls ~f:(fun xml ->
        match tag xml with
        | None ->
          false
        | Some tag2 ->
          let tag2 = String.lowercase tag2 in
          String.(tag1 = tag2))
  ;;

  let with_attr one key value xmls =
    let xmls = one xmls in
    List.filter xmls ~f:(fun xml ->
      match attr xml key with
      | None        -> false
      | Some value' -> String.(value = value'))
  ;;
end

let matches t query =
  Query.run query t
;;

let match_one_exn ~here t query =
  match Query.run query t with
  | [t] -> t
  | []  -> failwithf "query %s matched nothing" (Source_code_position.to_string here) ()
  | _   -> failwithf "query %s matched multiple" (Source_code_position.to_string here) ()
;;

let exists t query =
  not (List.is_empty (matches t query))
;;

let attr_exn ~here t key =
  match attr t key with
  | Some value ->
    value
  | None ->
    failwithf "no attr '%s' at %s" key (Source_code_position.to_string here) ()
;;

let text_content_exn ~here t =
  match t with
  | Xml.PCData text ->
    text
  | _ ->
    String.concat ~sep:" " (List.map (children t) ~f:(fun xml ->
      match xml with
      | Xml.PCData text ->
        text
      | _ ->
        failwithf "xml had non-text children at %s"
          (Source_code_position.to_string here) ()))
;;
