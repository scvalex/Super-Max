open Core.Std

let draw (_drawing : Drawing.t) =
  ()
;;

let run_test () =
  Sdl.init [`VIDEO];
  let (window, renderer) =
    Sdlrender.create_window_and_renderer
      ~width:0 ~height:0
      ~flags:[Sdlwindow.FullScreen_Desktop]
  in
  let (width, height) = Sdlwindow.get_size window in
  Printf.printf "Window size is (%d, %d)\n" width height;
  Sdlwindow.set_title ~window ~title:"Something romantic";
  Sdlrender.set_logical_size2 renderer ~x:width ~y:height;
  draw (Drawing.Example.rectangles ~width ~height);
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
