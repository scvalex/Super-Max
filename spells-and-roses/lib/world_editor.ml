open Core.Std
open Async.Std

let edit ~file =
  if not (Core.Std.Sys.is_file_exn file) then
    Printf.printf "Creating new world %s\n%!" file;
  Deferred.unit
;;
