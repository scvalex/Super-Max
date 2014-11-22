open Core.Std
open Async.Std

module X = Xml_helper
module Q = Xml_helper.Query

let split_words text =
  List.filter_map (String.split_on_chars text ~on:[' '; '\t'; '\n'; '\r'])
    ~f:(fun word ->
      let word = String.strip word in
      if String.is_empty word
      then None
      else Some word)
;;

let get_id ~here xml attr =
  let xpath = X.attr_exn ~here xml attr in
  match String.chop_prefix xpath ~prefix:"#" with
  | None    -> failwithf "can't handle attr %s: '%s'" attr xpath ()
  | Some id -> id
;;

let extract_mesh ~source ~source_id =
  Deferred.Or_error.try_with (fun () ->
    Xml_helper.parse_file_exn source
    >>| fun xml ->
    let node_xml =
      X.match_one_exn ~here:_here_ xml
        Q.( empty
            +> tag "library_visual_scenes"
            +> tag "visual_scene"
            +> with_attr (tag "node") "id" source_id )
    in
    (* CR scvalex: Apply tranformation matrix  to vertices. *)
    let mesh_id =
      let instance_geometry_xml =
        X.match_one_exn ~here:_here_ node_xml
          Q.( empty +> tag "instance_geometry")
      in
      get_id ~here:_here_ instance_geometry_xml "url"
    in
    let mesh_xml =
      X.match_one_exn ~here:_here_ xml
        Q.( empty
            +> tag "library_geometries"
            +> with_attr (tag "geometry") "id" mesh_id
            +> tag "mesh")
    in
    let position_xml =
      X.match_one_exn ~here:_here_ mesh_xml
        Q.( empty
            +> tag "vertices"
            +> with_attr (tag "input") "semantic" "POSITION" )
    in
    let source_id = get_id ~here:_here_ position_xml "source" in
    let material_id =
      let instance_material_xml =
        X.match_one_exn ~here:_here_ node_xml
          Q.( empty
              +> tag "instance_geometry"
              +> tag "bind_material"
              +> tag "technique_common"
              +> tag "instance_material" )
      in
      get_id ~here:_here_ instance_material_xml "target"
    in
    let polylist_xml =
      X.match_one_exn ~here:_here_ mesh_xml
        Q.( empty
            +> with_attr (tag "polylist") "material" material_id )
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
      let input_count =
        List.length (X.matches polylist_xml Q.(empty +> tag "input"))
      in
      let p_xml =
        X.match_one_exn ~here:_here_ polylist_xml Q.(empty +> tag "p")
      in
      let text = X.text_content_exn ~here:_here_ p_xml in
      let words = split_words text in
      let indices = List.map words ~f:Int32.of_string in
      List.filteri indices ~f:(fun idx _ -> Int.(idx mod input_count = 0))
    in
    let indices =
      Int_array.of_array (Array.of_list indices)
    in
    (positions, indices))
;;
