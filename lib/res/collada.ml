open Core.Std
open Async.Std

let extract_mesh ~source ~source_id:_ =
  let open Deferred.Or_error.Monad_infix in
  Xml_helper.parse_file source
  >>= fun _xml ->
  Deferred.return (Or_error.errorf "not implemented")
;;
