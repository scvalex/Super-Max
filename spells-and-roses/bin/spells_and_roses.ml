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
                   Tests.moving_rectangle )
             ; ( "rectangles",
                 Command.basic
                   ~summary:"Display a several static rectangles"
                   Command.Spec.empty
                   Tests.rectangles )
             ])
       ])
;;

let () = Exn.handle_uncaught ~exit:true main;;
