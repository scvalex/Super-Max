open Core.Std
open Async.Std
open Ocaml_plugin.Std

module World_plugin = Ocaml_compiler.Make(struct
  type t = (module World.S)
  let t_repr = "World.S";;
  let univ_constr = World.univ_constr;;
  let univ_constr_repr = "World.univ_constr";;
end)

let load ~file =
  World_plugin.load_ocaml_src_files [file]
  |! Deferred.Or_error.ok_exn
;;

type state = {
  camera_x : float;
  camera_y : float;
}

let create () =
  let camera_x = 0.0 in
  let camera_y = 0.0 in
  { camera_x; camera_y; }
;;

let on_event t ev =
    match ev with
  | Sdlevent.Quit _
  | Sdlevent.KeyUp {Sdlevent. keycode = Sdlkeycode.Q; _} ->
    `Quit
  | _ ->
    `Continue t
;;

let on_step t =
  `Continue t
;;

let edit ~file ~data_dir =
  load ~file
  >>= fun world ->
  let module W = (val world : World.S) in
  Game.with_sdl ~data_dir ~f:(fun ~ctx ~width ~height ->
      (* CR scvalex: This shouldn't be W.create; it should be some
         static, auto-generated submodule. *)
      let world_state = W.create ~width ~height in
      let initial_state = create () in
      let steps_per_sec = 60.0 in
      let drawing_of_state t =
        World.to_drawing (W.entities world_state) ~layers:W.layers
          ~camera:(`X t.camera_x, `Y t.camera_y)
      in
      Game.main_loop ~initial_state ~on_event ~on_step
        ~steps_per_sec ~drawing_of_state ~ctx)
;;
