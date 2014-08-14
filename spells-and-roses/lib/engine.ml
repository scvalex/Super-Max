open Core.Std

type 'u t = {
  updates          : 'u Queue.t;
  mutable quitting : bool;
}

let broadcast t update =
  Queue.enqueue t.updates update
;;

let quit t =
  t.quitting <- true
;;

module Internal = struct
  let create () =
    let updates = Queue.create () in
    let quitting = false in
    { updates; quitting; }
  ;;

  let drain_updates t =
    let updates = Queue.create () in
    Queue.blit_transfer ~src:t.updates ~dst:updates ();
    updates
  ;;

  let quitting t = t.quitting;;
end
