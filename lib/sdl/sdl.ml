(* https://wiki.libsdl.org/CategoryAPI *)

open Core_kernel.Std
open Ctypes
open Foreign

module UInt32 = Unsigned.UInt32
module Int32 = Signed.Int32
module UInt8 = Unsigned.UInt8

module Sdl = struct
  type window = unit ptr
  let window : window typ = ptr void
  let window_opt : window option typ = ptr_opt void

  type gl_context = unit ptr
  let gl_context : gl_context typ = ptr void

  module Event = struct
    module Window = struct
      type t
      let t : t structure typ = structure "SDL_WindowEvent"

      let _ = field t "type" uint32_t;;
      let _ = field t "timestamp" uint32_t;;
      let _ = field t "windowID" uint32_t;;
      let event = field t "event" uint8_t;;
      let _ = field t "data1" int32_t;;
      let _ = field t "data2" int32_t;;

      seal t;;
    end

    module Keysym = struct
      type t
      let t : t structure typ = structure "SDL_Keysym"

      let _ = field t "scancode" int32_t;;
      let sym = field t "sym" int32_t;;
      let _ = field t "mod" uint16_t;;
      let _ = field t "unused" uint32_t;;

      seal t;;
    end

    module Keyboard = struct
      type t
      let t : t structure typ = structure "SDL_KeyboardEvent"

      let _ = field t "type" uint32_t;;
      let _ = field t "timestamp" uint32_t;;
      let _ = field t "windowID" uint32_t;;
      let _ = field t "state" uint8_t;;
      let _ = field t "repeat" uint8_t;;
      let keysym = field t "keysym" Keysym.t;;

      seal t;;
    end

    type t
    let t : t union typ = union "SDL_Event"

    let type_ = field t "type" uint32_t;;

    let window = field t "window" Window.t;;

    let key = field t "key" Keyboard.t;;

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

type key =
  [ `Down | `Up | `Left | `Right
  | `Unknown of int
  ] with sexp

type event =
  [ `Quit
  | `Unknown of string
  | `Key of ([`Down | `Up] * key)
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
  let event_type = UInt32.to_int (getf event Event.type_) in
  match event_type with
  | 0x100 ->
    `Quit
  | 0x200 -> begin
      let window_event = getf event Event.window in
      match UInt8.to_int (getf window_event Event.Window.event) with
      | 14 -> `Quit
      | n  -> `Unknown (sprintf "unknown window event: %d" n)
    end
  | 0x300 | 0x301 -> begin
      let key_event = getf event Event.key in
      let dir = if Int.(event_type = 0x300) then `Down else `Up in
      let sym =
        let sym =
          Int32.to_int (getf (getf key_event Event.Keyboard.keysym) Event.Keysym.sym)
        in
        match sym with
        | 1073741903 -> `Right
        | 1073741904 -> `Left
        | 1073741905 -> `Down
        | 1073741906 -> `Up
        | n          -> `Unknown n
      in
      `Key (dir, sym)
    end
  | n     -> `Unknown (sprintf "unknown event type: %d" n)
;;

let poll_event () =
  let event = make Event.t in
  match poll_event (addr event) with
  | 0 -> None
  | 1 -> Some (interpret_event event)
  | n -> failwithf "poll_event returned %d" n ()
;;
