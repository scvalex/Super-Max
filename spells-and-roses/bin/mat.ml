open Core.Std

type mat3 = float array array with sexp

type vec3 = float array with sexp

let zero () =
  [| [| 0.0; 0.0; 0.0 |]
   ; [| 0.0; 0.0; 0.0 |]
   ; [| 0.0; 0.0; 0.0 |]
  |]
;;

let id () =
  [| [| 1.0; 0.0; 0.0 |]
   ; [| 0.0; 1.0; 0.0 |]
   ; [| 0.0; 0.0; 1.0 |]
  |]
;;

let scale ~x ~y =
  [| [|   x; 0.0; 0.0 |]
   ; [| 0.0;   y; 0.0 |]
   ; [| 0.0; 0.0; 1.0 |]
  |]
;;

let translate ~x ~y =
  [| [| 1.0; 0.0;   x |]
   ; [| 0.0; 1.0;   y |]
   ; [| 0.0; 0.0; 1.0 |]
  |]
;;

let ( * ) m1 m2 =
  let m3 = zero () in
  for i = 0 to 2 do
    for j = 0 to 2 do
      for k = 0 to 2 do
        m3.(i).(j) <- m3.(i).(j) +. m1.(i).(k) *. m2.(k).(j)
      done
    done
  done;
  m3
;;

let ( *| ) mat vec =
  [| mat.(0).(0) *. vec.(0) +. mat.(0).(1) *. vec.(1) +. mat.(0).(2) *. vec.(2)
   ; mat.(1).(0) *. vec.(0) +. mat.(1).(1) *. vec.(1) +. mat.(1).(2) *. vec.(2)
   ; mat.(2).(0) *. vec.(0) +. mat.(2).(1) *. vec.(1) +. mat.(2).(2) *. vec.(2)
  |]
;;

let vec3_of_xy ~x ~y =
  [| x; y; 1.0 |]
;;

let xy_of_vec3 vec =
  (vec.(0), vec.(1))
;;
