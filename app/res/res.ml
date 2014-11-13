open Core.Std
open Async.Std
open Res_lib.Std

let extract_mesh ~source ~source_id ~target_id =
  Extract.extract_mesh ~source ~source_id ~target_id
  |> Deferred.Or_error.ok_exn
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
            ])
       ])
;;

let () = Exn.handle_uncaught ~exit:true main;;
