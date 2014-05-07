open Core.Std
open Async.Std

module Flag = struct
  include Command.Spec

  let data_dir =
    flag "data-dir" (optional_with_default "resources" file)
      ~doc:"DIR location of game resources"
  ;;
end

let main () =
  Command.run
    (Command.group ~summary:"Spells and Roses"
       [ ( "test",
           Command.group ~summary:"System tests"
             [ ( "moving-rectangle",
                 Command.async_basic
                   ~summary:"Display a white rectangle moving horizontally"
                   Flag.
                     ( empty
                       +> data_dir )
                   (fun data_dir () ->
                      Tests.Moving_rectangle.run ~data_dir) )
             ; ( "rectangles",
                 Command.async_basic
                   ~summary:"Display a several static rectangles"
                   Flag.
                     ( empty
                       +> data_dir )
                   (fun data_dir () ->
                      Tests.Rectangles.run ~data_dir) )
             ; ( "text",
                 Command.async_basic
                   ~summary:"Display static text"
                   Flag.
                     ( empty
                       +> data_dir )
                   (fun data_dir () ->
                      Tests.Static_text.run ~data_dir) )
             ; ( "dancing-banana",
                 Command.async_basic
                   ~summary:"Display a dancing banana"
                   Flag.
                     ( empty
                       +> data_dir )
                   (fun data_dir () ->
                      Tests.Dancing_banana.run ~data_dir) )
             ; ( "psychedelic-cat",
                 Command.async_basic
                   ~summary:"Display a very special cat"
                   Flag.
                     ( empty
                       +> data_dir )
                   (fun data_dir () ->
                      Tests.Psy_cat.run ~data_dir) )
             ])
       ; ("edit",
          Command.group ~summary:"Editor"
            [ ("world",
               Command.async_basic
                 ~summary:"Edit a world"
                 Flag.
                   ( empty
                     +> anon ("FILE" %: file) )
                 (fun file () ->
                    World_editor.edit ~file))
            ])
       ])
;;

let () = Exn.handle_uncaught ~exit:true main;;
