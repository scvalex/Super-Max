open Core.Std
open Async.Std

let extract_mesh ~source:_ ~source_id:_ =
  Deferred.return (Or_error.errorf "not implemented")
;;
