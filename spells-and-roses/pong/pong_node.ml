open Core.Std

module State = struct
  module Player = struct
    type t = A | B with sexp, compare
  end

  module Direction = struct
    type t = Up | Down with sexp, compare
  end

  module Pong_event = struct
    type t =
      | Player_join of Player.t
      | Move of (Player.t * Direction.t)
    with sexp, compare
  end

  module Event = struct
    module T = struct
      type t = {
        source : Node.Id.t;
        step   : int;
        event  : Pong_event.t;
      } with fields, sexp, compare
    end

    include T
    include Comparable.Make(T)
  end

  type t = unit

  let create () =
    ()
  ;;

  let on_step t =
    t
  ;;

  let on_event t _ev =
    t
  ;;
end

module Node = Node.Make(State)

type t = Node.t

let create () =
  Node.create ~step:0 ~state:(State.create ()) ~history_length:60
;;
