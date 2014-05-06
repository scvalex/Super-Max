open Core.Std

let edit ~file =
  if not (Sys.is_file_exn file) then
    Printf.printf "Creating new world %s\n%!" file;
;;
