open Core.Std

module Id = String_id

module On_disk = struct
  type t = {
    id : Id.t;
  } with sexp
end

type 'w t = {
  id         : 'w -> Id.t;
  to_drawing : 'w -> Drawing.t;
  on_step    : 'w -> ('w t * 'w);
  on_event   : 'w -> Sdlevent.t -> ('w t * 'w);
}

let rec create ~id ~to_drawing ~on_step ~on_event ~state =
  let id' world = id state world in
  let to_drawing' world = to_drawing state world in
  let on_step' world =
    let (state, world) = on_step state world in
    (create ~id ~to_drawing ~on_step ~on_event ~state, world)
  in
  let on_event' world event =
    let (state, world) = on_event state world event in
    (create ~id ~to_drawing ~on_step ~on_event ~state, world)
  in
  {
    id         = id';
    to_drawing = to_drawing';
    on_step    = on_step';
    on_event   = on_event';
  }
;;

let id t world = t.id world;;

let to_drawing t world = t.to_drawing world;;

let on_step t world = t.on_step world;;

let on_event t world event = t.on_event world event;;
