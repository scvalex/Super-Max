open Core.Std

module Id = String_id

module On_disk = struct
  type t = {
    id : Id.t;
  } with sexp
end

type t = {
  id         : unit -> Id.t;
  to_drawing : unit -> Drawing.t;
  on_step    : unit -> t;
  on_event   : Sdlevent.t -> t;
}

let rec create ~id ~to_drawing ~on_step ~on_event ~state =
  let id' () = id state in
  let to_drawing' () = to_drawing state in
   let on_step' () =
    let state = on_step state in
    create ~id ~to_drawing ~on_step ~on_event ~state
  in
  let on_event' event =
    let state = on_event state event in
    create ~id ~to_drawing ~on_step ~on_event ~state
  in
  {
    id         = id';
    to_drawing = to_drawing';
    on_step    = on_step';
    on_event   = on_event';
  }
;;

let id t = t.id ();;

let to_drawing t = t.to_drawing ();;

let on_step t = t.on_step ();;

let on_event t event = t.on_event event;;
