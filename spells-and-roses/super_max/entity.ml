open Core.Std

module Id = String_id

type 'w t = {
  id         : Id.t;
  to_drawing : 'w -> Drawing.t;
  on_step    : 'w -> ('w t * 'w);
  on_event   : 'w -> Sdlevent.t -> ('w t * 'w);
} with fields

let rec create ~id ~to_drawing ~on_step ~on_event ~state =
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
    id;
    to_drawing = to_drawing';
    on_step    = on_step';
    on_event   = on_event';
  }
;;

let to_drawing t world = t.to_drawing world;;

let on_step t world = t.on_step world;;

let on_event t world event = t.on_event world event;;
