open Core.Std
open Async.Std

let split_words text =
  List.filter_map (String.split_on_chars text ~on:[' '; '\t'; '\n'; '\r'])
    ~f:(fun word ->
      let word = String.strip word in
      if String.is_empty word
      then None
      else Some word)
;;

let extract_mesh ~source ~source_id =
  let module X = Xml_helper in
  let module Q = Xml_helper.Query in
  Deferred.Or_error.try_with (fun () ->
    Xml_helper.parse_file_exn source
    >>| fun xml ->
    let mesh_xml =
      X.match_one_exn ~here:_here_ xml
        Q.( empty
            +> tag "library_geometries"
            +> with_attr (tag "geometry") "id" source_id
            +> tag "mesh")
    in
    let position_xml =
      X.match_one_exn ~here:_here_ mesh_xml
        Q.( empty
            +> tag "vertices"
            +> with_attr (tag "input") "semantic" "POSITION" )
    in
    let source_id =
      let xpath = X.attr_exn ~here:_here_ position_xml "source" in
      match String.chop_prefix xpath ~prefix:"#" with
      | None    -> failwithf "can't handle source_id '%s'" xpath ()
      | Some id -> id
    in
    let polylist_xml =
      let matches =
        List.filter (X.matches mesh_xml Q.(empty +> tag "polylist"))
          ~f:(fun polylist_xml ->
            X.exists polylist_xml
              Q.( empty
                  +> with_attr (tag "input") "semantic" "VERTEX") )
      in
      match matches with
      | [polylist_xml] -> polylist_xml
      | _              -> failwithf "wrong number of polylists: %d"
                            (List.length matches) ()
    in
    let positions_xml =
      X.match_one_exn ~here:_here_ mesh_xml
        Q.( empty
            +> with_attr (tag "source") "id" source_id )
    in
    (* CR scvalex: Validate the technique of the positions. *)
    (* CR scvalex: Convert units to meters (if not already in meters). *)
    let positions =
      let float_array_xml =
        X.match_one_exn ~here:_here_ positions_xml
          Q.(empty +> tag "float_array")
      in
      let text = X.text_content_exn ~here:_here_ float_array_xml in
      let words = split_words text in
      List.map words ~f:Float.of_string
    in
    let positions =
      Float_array.of_array (Array.of_list positions)
    in
    let indices =
      let p_xml =
        X.match_one_exn ~here:_here_ polylist_xml
          Q.(empty +> tag "p")
      in
      let text = X.text_content_exn ~here:_here_ p_xml in
      let words = split_words text in
      List.map words ~f:Int32.of_string
    in
    let indices =
      Int_array.of_array (Array.of_list indices)
    in
    (positions, indices))
;;
