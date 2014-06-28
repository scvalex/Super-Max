open Core.Std

let m fmt =
  let print str =
    Printf.printf "%s: %s\n%!" (Time.to_string (Time.now ())) str
  in
  Printf.ksprintf print fmt
;;
