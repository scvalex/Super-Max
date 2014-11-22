open Core.Std
open Async.Std
open Res_lib.Std

let extract_mesh ~source ~source_id ~target_id =
  Extract.extract_mesh ~source ~source_id ~target_id
  |> Deferred.Or_error.ok_exn
;;

let extract_program ~vertex ~fragment ~target_id =
  Extract.extract_program ~vertex ~fragment ~target_id
  |> Deferred.Or_error.ok_exn
;;

let identify_res ~verbose ~file =
  let module Res = Res_lib.Std.Res in
  let (`Dir _, `Pack pack, `Name name) = Res_id.analyze_filename file in
  Res.load ~id:(Res_id.create ~pack ~name) file
  |> Deferred.Or_error.ok_exn
  >>| fun res ->
  let p fmt = ksprintf (fun str -> Printf.printf "%s\n%!" str) fmt in
  let pn fmt = ksprintf (fun str -> Printf.printf "%s" str) fmt in
  let print_lines ~prefix text =
    List.iter (String.split_lines text) ~f:(fun line ->
      p "%s%s" prefix line)
  in
  p "res-id: %s/%s" pack name;
  match Res.data res with
  | `Mesh mesh ->
    p "data: Mesh";
    p " - vertices: %d" (Float_array.length (Res.Mesh.positions mesh) / 3);
    if verbose then begin
      pn "    ";
      Float_array.iteri (Res.Mesh.positions mesh) ~f:(fun idx pos ->
        pn "%.2f%s" pos (if Int.(idx mod 3 = 2) then ", " else " "));
      p "";
    end;
    p " - indices:  %d" (Int_array.length (Res.Mesh.indices mesh));
    if verbose then begin
      pn "    ";
      Int_array.iteri (Res.Mesh.indices mesh) ~f:(fun _idx idx ->
        pn "%s " (Int32.to_string idx));
      p "";
    end
  | `Program program ->
    p "data: Program";
    let vertex = Res.Program.vertex program in
    p " - vertex shader: %d chars, %d lines"
      (String.length vertex) (List.length (String.split_lines vertex));
    if verbose then
      print_lines ~prefix:"   > " vertex;
    let fragment = Res.Program.fragment program in
    p " - fragment shader: %d chars, %d lines"
      (String.length fragment) (List.length (String.split_lines fragment));
    if verbose then
      print_lines ~prefix:"   > " fragment
;;

module Flag = struct
  include Command.Spec

  let source =
    flag "source" (required file)
      ~doc:"FILE where to extract the resource from"
  ;;

  let source_id =
    flag "source-id" (required string)
      ~doc:"STRING unique id of the resource in the source file"
  ;;

  let target_id =
    flag "target-id" (required string)
      ~doc:"STRING our id for the resource; also determines the output file"
  ;;

  let vertex =
    flag "vertex" (required file)
      ~doc:"FILE vertex shader source"
  ;;

  let fragment =
    flag "fragment" (required file)
      ~doc:"FILE fragment shader source"
  ;;

  let verbose =
    flag "verbose" no_arg
      ~doc:" Show verbose output"
end

let main () =
  Command.run
    (Command.group ~summary:"Resource tools"
       [ ("extract",
          Command.group ~summary:"Extract resources from foreign files"
            [ ("mesh",
               Command.async
                 ~summary:"Extract a mesh from a foreign file"
                 Flag.( empty +> source +> source_id +> target_id)
                 (fun source source_id target_id () ->
                    extract_mesh ~source ~source_id ~target_id))
            ; ("program",
               Command.async
                 ~summary:"Save a program's shaders to a res file"
                 Flag.( empty +> vertex +> fragment +> target_id)
                 (fun vertex fragment target_id () ->
                    extract_program ~vertex ~fragment ~target_id))
            ])
       ; ("identify",
          Command.async
            ~summary:"Identify a res file"
            Flag.( empty +> verbose +> anon ("FILE" %: file) )
            (fun verbose file () ->
               identify_res ~verbose ~file))
       ])
;;

let () = Exn.handle_uncaught ~exit:true main;;
