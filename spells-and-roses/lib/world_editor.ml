open Core.Std
open Async.Std
open Ocaml_plugin.Std

module World_plugin = Ocaml_compiler.Make(struct
  type t = (module World_intf.S)
  let t_repr = "World_intf.S";;
  let univ_constr = World_intf.univ_constr;;
  let univ_constr_repr = "World_intf.univ_constr";;
end)

let load ~file =
  World_plugin.load_ocaml_src_files [file]
  |! Deferred.Or_error.ok_exn
;;

let edit ~file ~data_dir =
  load ~file
  >>= fun world ->
  let module W = (val world : World_intf.S) in
  Game.with_sdl ~data_dir ~f:(fun ~ctx ~width ~height ->
      let world_state = W.create ~width ~height in
      let initial_state = () in
      let on_event state _event =
        `Continue state
      in
      let on_step state =
        `Continue state
      in
      let steps_per_sec = 60.0 in
      let drawing_of_state _state =
        World_intf.to_drawing (W.entities world_state)
          ~camera:(`X 0.0, `Y 0.0) ~layers:W.layers
      in
      Game.main_loop ~initial_state ~on_event ~on_step
        ~steps_per_sec ~drawing_of_state ~ctx)
;;
