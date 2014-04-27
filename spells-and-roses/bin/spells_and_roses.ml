open Core.Std

let draw ~width ~height (drawing : Drawing.t) =
  let _drawing =
    Drawing.(scale ~x:(1.0 /. width) ~y:(1.0 /. height) drawing)
  in
  ()
;;

let run_test () =
  Sdl.init [`VIDEO];
  let (width, height) = (320, 240) in
  let (_ : Sdlwindow.t) =
    Sdlwindow.create2
      ~title:"Let's try SDL2 with OCaml!"
      ~x:`undefined ~y:`undefined ~width ~height
      ~flags:[]
  in
  Sdltimer.delay ~ms:2000;
  Sdl.quit ()
;;

let main () =
  Command.run
    (Command.group ~summary:"Spells and Roses"
       [ ( "test",
           Command.basic
             ~summary:"system test"
             Command.Spec.
               ( empty )
             run_test )
       ])
;;

let () = Exn.handle_uncaught ~exit:true main;;
