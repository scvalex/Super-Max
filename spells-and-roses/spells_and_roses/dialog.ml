open Core.Std

module Simple_query = struct
  type 'a t = {
    event_map : (Sdlkeycode.t * 'a) list;
    query     : string;
    width     : float;
    height    : float;
    font      : string;
    size_pt   : int;
  }

  let create ~event_map ~query ~width ~height ~font ~size_pt =
    let width = Float.of_int width in
    let height = Float.of_int height in
    { event_map; query; width; height; font; size_pt; }
  ;;

  let on_event t ev =
    match ev with
    | Sdlevent.KeyUp {Sdlevent. keycode; _} ->
      List.Assoc.find t.event_map keycode
    | _ ->
      None
  ;;

  let to_drawing t =
    let open Drawing in
    let responses =
      String.concat ~sep:"/" (List.map t.event_map ~f:(fun (keycode, _) ->
          Sdlkeycode.to_string keycode))
    in
    many
      [ rectangle ~filled:false ~width:t.width ~height:t.height
      ; translate ~x:(t.width /. 2.0) ~y:(t.height /. 2.0)
          (text ~font:t.font ~size_pt:t.size_pt
             ~position:(`X `Centre, `Y `Centre)
             [t.query; responses])
      ]
  ;;
end
