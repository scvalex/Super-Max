open Core.Std

module Id = String_id

type ('a, 'c, 'ev) entity = {
  id         : Id.t;
  to_drawing : 'a -> Drawing.t;
  on_step    : 'a -> 'a;
  on_event   : 'a -> 'ev -> 'a;
  state      : 'a;
  common     : 'a -> 'c;
}

type ('c, 'ev) t =
  | Entity : ('a, 'c, 'ev) entity -> ('c, 'ev) t

let create ~id ~to_drawing ~on_step ~on_event ~state ~common =
  Entity { id; to_drawing; on_step; on_event; state; common}
;;

let id (Entity t) = t.id;;

let to_drawing (Entity t) =
  t.to_drawing t.state
;;

let on_step (Entity t) =
  let state = t.on_step t.state in
  Entity { t with state; }
;;

let on_event (Entity t) ev =
  let state = t.on_event t.state ev in
  Entity { t with state; }
;;

let common (Entity t) =
  t.common t.state
;;
