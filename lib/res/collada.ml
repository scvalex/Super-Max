open Core.Std let _ = _squelch_unused_module_warning_
open Async.Std

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
    let vertices_xml =
      X.match_one_exn ~here:_here_ mesh_xml
        Q.( empty
            +> with_attr (tag "source") "id" source_id )
    in
    (* CR scvalex: Validate the technique of the vertices. *)
    let vertices =
      let float_array_xml =
        X.match_one_exn ~here:_here_ vertices_xml
          Q.( empty
              +> tag "float_array" )
      in
      let text = X.text_content_exn ~here:_here_ float_array_xml in
      let words = String.split_on_chars text ~on:[' '; '\t'; '\n'; '\r'] in
      List.filter_map words ~f:(fun word ->
        let word = String.strip word in
        if String.is_empty word
        then None
        else Some (Float.of_string word))
    in
    Float_array.of_array (Array.of_list vertices))
;;
