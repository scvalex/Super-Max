open Core.Std

include Node_intf

exception Event_not_in_range of (int * int) with sexp
exception Step_not_in_history of int with sexp

module Make(State : State) = struct
  module Event = State.Event

  (* History semantics:
       (state_i, events_i), (state_i+1, _), ...
       state_i+1 = apply_events(state_i, events_i) *)
  type t = {
    step           : int;
    history_length : int;
    history        : (Event.Set.t *  State.t) Int.Map.t;
  }

  let create ~step ~state ~history_length =
    let history =
      Map.add Int.Map.empty ~key:step ~data:(Event.Set.empty, state)
    in
    { step; history_length; history; }
  ;;

  let apply_events state events =
    if Set.is_empty events
    then
      state
    else
      List.fold_left ~init:state (Set.to_list events) ~f:(fun state ev ->
          State.on_event state ev)
  ;;

  let rec recompute_history t ~after =
    if Int.(after >= t.step)
    then begin
      t
    end else begin
      let (events, state) =
        Option.value_exn ~here:_here_ (Map.find t.history after)
      in
      let state' = State.on_step (apply_events state events) in
      let history =
        Map.change t.history (after + 1) (fun history_entry ->
            let (events', _) = Option.value_exn ~here:_here_ history_entry in
            Some (events', state'))
      in
      recompute_history { t with history; } ~after:(after + 1)
    end
  ;;

  let add_event t ev =
    (* We only accept events at steps in interval [[lo, hi]]. *)
    let (lo, hi) = (t.step - t.history_length, t.step) in
    let ev_step = Event.step ev in
    if Int.(ev_step < lo || hi < ev_step)
    then begin
      Or_error.of_exn (Event_not_in_range (lo, hi))
    end else begin
      match Map.find t.history ev_step with
      | None ->
        Or_error.of_exn (Step_not_in_history ev_step)
      | Some (events, state) ->
        let events = Set.add events ev in
        let history = Map.add t.history ~key:ev_step ~data:(events, state) in
        let t = { t with history; } in
        Ok (recompute_history t ~after:ev_step)
    end
  ;;

  let on_step t =
    let (events, state) =
      Option.value_exn ~here:_here_ (Map.find t.history t.step)
    in
    let state' = State.on_step (apply_events state events) in
    let step' = t.step + 1 in
    let history =
      Map.add ~key:step' ~data:(Event.Set.empty, state')
        (Map.remove t.history (t.step - t.history_length))
    in
    { t with step = step'; history; }
  ;;

  let state t =
    let (_, state) =
      Option.value_exn ~here:_here_ (Map.find t.history t.step)
    in
    state
  ;;
end
