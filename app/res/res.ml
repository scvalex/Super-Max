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

let identify_res ~file =
  let module Res = Res_lib.Std.Res in
  Res.load ~id:(Res_id.create ~pack:"unkown" ~name:"mystery") file
  |> Deferred.Or_error.ok_exn
  >>| fun res ->
  let p fmt = ksprintf (fun str -> Printf.printf "%s\n%!" str) fmt in
  match Res.data res with
  | `Mesh mesh ->
    p "Mesh";
    p " - vertices: %d" (Float_array.length (Res.Mesh.positions mesh) / 3);
    p " - indices:  %d" (Int_array.length (Res.Mesh.indices mesh))
  | `Program _program ->
    p "Program"
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
            Flag.( empty +> anon ("FILE" %: file) )
            (fun file () ->
               identify_res ~file))
       ])
;;

let () = Exn.handle_uncaught ~exit:true main;;
