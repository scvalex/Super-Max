open Core.Std

let edit ~file =
  if not (Sys.is_file_exn file) then
    Printf.printf "Creating new map %s\n%!" file;
;;
