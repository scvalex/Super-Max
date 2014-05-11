open Core.Std

module Id = String_id

type ('a, 'w) t = {
  id         : Id.t;
  to_drawing : 'a -> 'w -> Drawing.t;
  on_step    : 'a -> 'w -> ('a * 'w);
  on_event   : 'a -> 'w -> Sdlevent.t -> ('a * 'w);
  state      : 'a;
} with fields

let create = Fields.create;;

let to_drawing t world =
  t.to_drawing t.state world
;;

let on_step t world =
  let (state, world) = t.on_step t.state world in
  ({ t with state; }, world)
;;

let on_event t world event =
  let (state, world) = t.on_event t.state world event in
  ({ t with state; }, world)
;;
