open Core.Std
open Async.Std

let target ~target_id =
  target_id ^ ".res"
;;

let (>>=?) = Deferred.Or_error.(>>=);;

let extract_mesh ~source ~source_id ~target_id =
  let extract_data () =
    match Filename.split_extension source with
    | (_, Some "dae") ->
      Collada.extract_mesh ~source ~source_id
    | _ ->
      Deferred.return (Or_error.errorf "unknown file format: %s" source)
  in
  extract_data ()
  >>=? fun (positions, indices) ->
  let id = Res_id.create ~pack:"unknown" ~name:target_id in
  let res = Res.create_mesh ~source ~source_id ~positions ~indices id in
  Res.save res (target ~target_id)
;;

let extract_program ~vertex ~fragment ~target_id =
  Deferred.Or_error.try_with (fun () ->
    Reader.file_contents vertex
    >>= fun vertex ->
    Reader.file_contents fragment
    >>| fun fragment ->
    (vertex, fragment))
  >>=? fun (vertex, fragment) ->
  let id = Res_id.create ~pack:"unknown" ~name:target_id in
  let res = Res.create_program ~vertex ~fragment id in
  Res.save res (target ~target_id)
;;
