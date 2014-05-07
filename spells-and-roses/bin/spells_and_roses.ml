open Core.Std
open Async.Std

let main () =
  Command.run
    (Command.group ~summary:"Spells and Roses"
       [ ( "test",
           Command.group ~summary:"System tests"
             [ ( "moving-rectangle",
                 Command.async_basic
                   ~summary:"Display a white rectangle moving horizontally"
                   Command.Spec.empty
                   Tests.Moving_rectangle.run )
             ; ( "rectangles",
                 Command.async_basic
                   ~summary:"Display a several static rectangles"
                   Command.Spec.empty
                   Tests.Rectangles.run )
             ; ( "text",
                 Command.async_basic
                   ~summary:"Display static text"
                   Command.Spec.empty
                   Tests.Static_text.run )
             ; ( "dancing-banana",
                 Command.async_basic
                   ~summary:"Display a dancing banana"
                   Command.Spec.empty
                   Tests.Dancing_banana.run )
             ; ( "psychedelic-cat",
                 Command.async_basic
                   ~summary:"Display a very special cat"
                   Command.Spec.empty
                   Tests.Psy_cat.run )
             ])
       ; ("edit",
          Command.group ~summary:"Editor"
            [ ("world",
               Command.async_basic
                 ~summary:"Edit a world"
                 Command.Spec.
                   ( empty
                     +> anon ("FILE" %: file) )
                 (fun file () ->
                    World_editor.edit ~file))
            ])
       ])
;;

let () = Exn.handle_uncaught ~exit:true main;;
