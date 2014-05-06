open Core.Std

let main () =
  Command.run
    (Command.group ~summary:"Spells and Roses"
       [ ( "test",
           Command.group ~summary:"System tests"
             [ ( "moving-rectangle",
                 Command.basic
                   ~summary:"Display a white rectangle moving horizontally"
                   Command.Spec.empty
                   Tests.Moving_rectangle.run )
             ; ( "rectangles",
                 Command.basic
                   ~summary:"Display a several static rectangles"
                   Command.Spec.empty
                   Tests.Rectangles.run )
             ; ( "text",
                 Command.basic
                   ~summary:"Display static text"
                   Command.Spec.empty
                   Tests.Static_text.run )
             ; ( "dancing-banana",
                 Command.basic
                   ~summary:"Display a dancing banana"
                   Command.Spec.empty
                   Tests.Dancing_banana.run )
             ; ( "psychedelic-cat",
                 Command.basic
                   ~summary:"Display a very special cat"
                   Command.Spec.empty
                   Tests.Psy_cat.run )
             ])
       ; ("edit",
          Command.group ~summary:"Editor"
            [ ("map",
               Command.basic
                 ~summary:"Edit a map"
                 Command.Spec.
                   ( empty
                     +> anon ("FILE" %: file) )
                 (fun file () ->
                    Map_editor.edit ~file))
            ])
       ])
;;

let () = Exn.handle_uncaught ~exit:true main;;
