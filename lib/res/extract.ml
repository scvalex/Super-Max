open Core.Std
open Async.Std

let extract_mesh ~source ~source_id ~target_id =
  let open Deferred.Or_error.Monad_infix in
  let target = target_id ^ ".res" in
  let extract_data () =
    match Filename.split_extension source with
    | (_, Some "dae") ->
      Collada.extract_mesh ~source ~source_id
    | _ ->
      Deferred.return (Or_error.errorf "unknown file format: %s" source)
  in
  extract_data ()
  >>= fun positions ->
  let res = Res.create_mesh ~source ~source_id ~positions () in
  Res.save res target
;;
