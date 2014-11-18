open Core.Std
open Async.Std
open Sdl_lib.Std
open Gl_lib.Std
open Res_lib.Std

module Ui : sig
  type 'a t

  val create : (unit -> 'a) -> 'a t

  val to_f : 'a t -> (unit -> 'a)
end = struct
  type 'a t = (unit -> 'a)

  let create f = f;;

  let to_f t = t;;
end

type t = {
  thread : In_thread.Helper_thread.t;
  window : Sdl.window;
}

let on_ui_thread t ui =
  In_thread.run ~thread:t.thread (Ui.to_f ui)
;;

let with_renderer f =
  let thread =
    Or_error.ok_exn
      (In_thread.Helper_thread.create ~name:"renderer" ())
  in
  In_thread.run ~thread (fun () ->
    Sdl.init ();
    Sdl.gl_set_attribute `Context_major_version 3;
    Sdl.gl_set_attribute `Context_major_version 3;
    Sdl.gl_set_attribute `Doublebuffer 1;
    Sdl.gl_set_attribute `Depthsize 24;
    let window = Sdl.create_window ~title:"Rock" in
    let gl_context = Sdl.gl_create_context window in
    (window, gl_context))
  >>= fun (window, gl_context) ->
  let t = { window; thread; } in
  Monitor.protect (fun () -> f t)
    ~finally:(fun () ->
      on_ui_thread t (Ui.create (fun () ->
        Sdl.gl_delete_context gl_context;
        Sdl.destroy_window window;
        Sdl.quit ())))
;;

let create_shader shader_type shader_code =
  let shader = Gl.create_shader shader_type in
  Gl.shader_source shader shader_code;
  Gl.compile_shader shader;
  let status = Gl.get_shader_iv shader `Compile_status in
  if Int.(status = 0) then begin
    let info_log_length = Gl.get_shader_iv shader `Info_log_length in
    let info = Gl.get_shader_info_log shader ~max_length:info_log_length in
    failwithf "failed to compile shader: %s" info ()
  end;
  shader
;;

let create_program shaders =
  let program = Gl.create_program () in
  List.iter shaders ~f:(Gl.attach_shader program);
  Gl.link_program program;
  let status = Gl.get_program_iv program `Link_status in
  if Int.(status = 0) then begin
    let info_log_length = Gl.get_program_iv program `Info_log_length in
    let info = Gl.get_program_info_log program ~max_length:info_log_length in
    failwithf "failed to link program: %s" info ()
  end;
  List.iter shaders ~f:(Gl.detach_shader program);
  program
;;

let test t =
  on_ui_thread t (Ui.create (fun () ->
    let vertex_positions =
      Float_array.of_array
        [| 0.75; 0.75; 0.0
         ; 0.75; -0.75; 0.0
         ; -0.75; -0.75; 0.0
        |]
    in
    let vertex_shader_code =
      "#version 330

layout(location = 0) in vec3 position;
void main() {
  gl_Position = vec4(position.xyz, 1.0);
}"
    in
    let fragment_shader_code =
      "#version 330

out vec4 outputColor;
void main() {
  outputColor = vec4(1.0f, 1.0f, 1.0f, 1.0f);
}"
    in
    let vertex_shader = create_shader `Vertex_shader vertex_shader_code in
    let fragment_shader = create_shader `Fragment_shader fragment_shader_code in
    let the_program = create_program [ vertex_shader; fragment_shader ] in
    Gl.delete_shader vertex_shader;
    Gl.delete_shader fragment_shader;
    let vao = Gl.gen_vertex_array ()in
    Gl.bind_vertex_array vao;
    let position_buffer_object = Gl.gen_buffer () in
    Gl.with_bound_buffer `Array_buffer position_buffer_object ~f:(fun () ->
      Gl.buffer_data `Array_buffer vertex_positions `Static_draw);
    Gl.clear_color 0.1 0.1 0.1 1.0;
    Gl.clear `Color_buffer_bit;
    Gl.with_used_program the_program ~f:(fun () ->
      Gl.with_bound_buffer `Array_buffer position_buffer_object ~f:(fun () ->
        Gl.with_vertex_attrib_array 0 ~f:(fun () ->
          Gl.vertex_attrib_pointer 0 ~size:3 `Float ~normalize:false ~stride:0;
          Gl.draw_arrays `Triangles ~first:0 ~count:3)));
    Sdl.gl_swap_window t.window))
  >>= fun () ->
  Clock.after (sec 3.0)
;;

let render_mesh _t _mesh =
  Ui.create (fun () ->
    ())
;;
