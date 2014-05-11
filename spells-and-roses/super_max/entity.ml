open Core.Std

module Id = String_id

type ('a, 'c, 'w) entity = {
  id         : Id.t;
  to_drawing : 'a -> 'w -> Drawing.t;
  on_step    : 'a -> 'w -> ('a * 'w);
  on_event   : 'a -> 'w -> Sdlevent.t -> ('a * 'w);
  state      : 'a;
  common     : 'a -> 'c;
}

type ('c, 'w) t =
  | Entity : ('a, 'c, 'w) entity -> ('c, 'w) t

let create ~id ~to_drawing ~on_step ~on_event ~state ~common =
  Entity { id; to_drawing; on_step; on_event; state; common}
;;

let id (Entity t) = t.id;;

let to_drawing (Entity t) world =
  t.to_drawing t.state world
;;

let on_step (Entity t) world =
  let (state, world) = t.on_step t.state world in
  (Entity { t with state; }, world)
;;

let on_event (Entity t) world event =
  let (state, world) = t.on_event t.state world event in
  (Entity { t with state; }, world)
;;

let common (Entity t) =
  t.common t.state
;;
