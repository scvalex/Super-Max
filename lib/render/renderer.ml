open Core.Std
open Async.Std
open Sdl_lib.Std
open Gl_lib.Std
open Res_lib.Std

type t = {
  thread : In_thread.Helper_thread.t;
  window : Sdl.window;
}

let on_ui_thread t f =
  In_thread.run ~thread:t.thread f
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
      on_ui_thread t (fun () ->
        Sdl.gl_delete_context gl_context;
        Sdl.destroy_window window;
        Sdl.quit ()))
;;

let test t =
  on_ui_thread t (fun () ->
    let _vertex_positions =
      (* CR scvalex: 4D positins here. *)
      Float_array.of_array
        [| 0.75; 0.75; 0.0; 1.0
         ; 0.75; -0.75; 0.0; 1.0
         ; -0.75; -0.75; 0.0; 1.0
        |]
    in
    let vertex_shader_code =
      "#version 330

layout(location = 0) in vec4 position;
void main() {
  gl_position = position;
}"
    in
    let fragment_shader_code =
      "#version 330

out vec4 outputColor;
void main() {
  outputColor = vec4(1.0f, 1.0f, 1.0f, 1.0f);
}"
    in
    let create_shader shader_type _shader_code =
      let shader = Gl.create_shader shader_type in
      (*   Gl.shader_source shader 1 shader_code `Null; *)
      (*   Gl.compile_shader shader; *)
      (*   let ok = Gl.get_shader_iv shade `Compile_status in *)
      (*   if not ok then begin *)
      (*     let info_log_length = Gl.get_shader_iv shader `Info_log_length in *)
      (*     let info = Gl.get_shader_info_log shader info_log_length None in *)
      (*     failwithf "failed to compile shader: %s" info () *)
      (*   end; *)
      shader
    in
    let create_program _shaders =
      let program = Gl.create_program () in
      (*   List.iter shaders ~f:(Gl.attach_shader program); *)
      (*   Gl.link_program program; *)
      (*   let ok = Gl.get_program_iv program `Link_status in *)
      (*   if not ok then begin *)
      (*     let info_log_length = Gl.get_program_iv program `Info_log_length in *)
      (*     let info = Gl.get_program_info_log program info_log_length None in *)
      (*     failwithf "failed to link program: %s" info () *)
      (*   end; *)
      (*   List.iter shaders ~f:(Gl.detach_shader program); *)
      program
    in
    let vertex_shader = create_shader `Vertex_shader vertex_shader_code in
    let fragment_shader = create_shader `Fragment_shader fragment_shader_code in
    let the_program = create_program [ vertex_shader; fragment_shader ] in
    Gl.delete_shader vertex_shader;
    Gl.delete_shader fragment_shader;
    let position_buffer_object = Gl.gen_buffer () in
    Gl.with_bound_buffer `Array_buffer position_buffer_object ~f:(fun () ->
      ()
      (* Gl.buffer_data `Array_buffer (Float_array.size_bytes vertex_positions) `Static_draw; *));
    Gl.clear_color 0.1 0.1 0.1 1.0;
    Gl.clear `Color_buffer_bit;
    Gl.with_used_program the_program ~f:(fun () ->
      Gl.with_bound_buffer `Array_buffer position_buffer_object ~f:(fun () ->
        ()
        (* Gl.enable_vertex_attrib_array 0; *)
        (* Gl.vertex_attrib_pointer 0 4 `Float false 0 0; *)
        (* Gl.draw_arrays `Triangles 0 3; *)
        (* Gl.disable_vertex_attrib_array 0; *)));
    Sdl.gl_swap_window t.window)
  >>= fun () ->
  Clock.after (sec 3.0)
;;
