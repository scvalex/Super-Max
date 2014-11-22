(* https://wiki.libsdl.org/CategoryAPI *)

open Core_kernel.Std
open Ctypes
open Foreign

module UInt32 = Unsigned.UInt32

module Sdl = struct
  type window = unit ptr
  let window : window typ = ptr void
  let window_opt : window option typ = ptr_opt void

  type gl_context = unit ptr
  let gl_context : gl_context typ = ptr void

  module Event = struct
    module Quit = struct
      type t
      let t : t structure typ = structure "SDL_QuitEvent"

      let _ = field t "type" uint32_t;;
      let _ = field t "timestamp" uint32_t;;

      seal t;;
    end

    type t
    let t : t union typ = union "SDL_Event"

    let type_ = field t "type" uint32_t;;

    let _quit = field t "quit" Quit.t;;

    let _padding = field t "padding" (array 56 uint8_t);;

    seal t;;
  end

  let write_never _ =
    failwith "write_never"
  ;;

  let get_error () =
    Error.of_string (foreign "SDL_GetError" (void @-> returning string) ())
  ;;

  let zero_to_ok =
    let read = function
      | 0   -> Ok ()
      | _   -> Error (get_error ())
    in
    view ~read ~write:write_never int
  ;;

  let some_to_ok t =
    let read = function
      | Some v -> Ok v
      | None   -> Error (get_error ())
    in
    view ~read ~write:write_never t
  ;;

  let init =
    foreign "SDL_Init" (uint32_t @-> returning zero_to_ok)
  ;;

  let quit =
    foreign "SDL_Quit" (void @-> returning void)
  ;;

  let gl_set_attribute =
    foreign "SDL_GL_SetAttribute" (int @-> int @-> returning zero_to_ok)
  ;;

  let create_window =
    foreign "SDL_CreateWindow"
      (string
       @-> int @-> int @-> int @-> int
       @-> uint32_t
       @-> returning (some_to_ok window_opt))
  ;;

  let destroy_window =
    foreign "SDL_DestroyWindow" (window @-> returning void)
  ;;

  let delay =
    foreign "SDL_Delay" (uint32_t @-> returning void)
  ;;

  let gl_create_context =
    foreign "SDL_GL_CreateContext" (window @-> returning gl_context)
  ;;

  let gl_delete_context =
    foreign "SDL_GL_DeleteContext" (gl_context @-> returning void)
  ;;

  let gl_swap_window =
    foreign "SDL_GL_SwapWindow" (window @-> returning void)
  ;;

  let poll_event =
    foreign "SDL_PollEvent" (ptr Event.t @-> returning int)
  ;;
end

include Sdl

type event =
  [ `Quit
  | `Unknown of string
  ] with sexp

let init () =
  let audio = 0x00000010 in
  let video = 0x00000020 in
  let events = 0x00004000 in
  Sdl.init (Unsigned.UInt32.of_int (audio lor video lor events))
  |> Or_error.ok_exn
;;

let gl_set_attribute attr value =
  let attr =
    match attr with
    | `Doublebuffer          -> 5
    | `Depthsize             -> 6
    | `Context_major_version -> 17
    | `Context_minor_version -> 18
  in
  Sdl.gl_set_attribute attr value
  |> Or_error.ok_exn
;;

let create_window ~title =
  let undefined = 0x1FFF0000 in
  let opengl = 0x00000002 in
  let fullscreen = 0x00000001 in
  (* CR scvalex: Why doesn't [fullscreen_desktop] work? *)
  (* let fullscreen_desktop = 0x00001000 lor fullscreen in *)
  Sdl.create_window title undefined undefined 1600 900
    (Unsigned.UInt32.of_int (opengl lor fullscreen))
  |> Or_error.ok_exn
;;

let delay ~ms =
  Sdl.delay (Unsigned.UInt32.of_int ms)
;;

let interpret_event event =
  match UInt32.to_int (getf event Event.type_) with
  | 0x100 -> `Quit
  | n     -> `Unknown (sprintf "unknown event type: %d" n)
;;

let poll_event () =
  let event = make Event.t in
  match poll_event (addr event) with
  | 0 -> None
  | 1 -> Some (interpret_event event)
  | n -> failwithf "poll_event returned %d" n ()
;;
