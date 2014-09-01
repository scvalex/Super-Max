open Core.Std
open Async.Std

module Flag = struct
  include Command.Spec

  let data_dir =
    flag "data-dir" (optional_with_default "resources" file)
      ~doc:"DIR location of game resources"
  ;;

  let player_file =
    anon ("PLAYER-FILE" %: file)
  ;;

  let host =
    flag "server" (required (Command.Spec.Arg_type.create Host_and_port.of_string))
      ~doc:"HOST pong server address"
  ;;
end

let test_command ~summary f =
  Command.async_basic ~summary
    Flag.(empty +> data_dir)
    (fun data_dir () ->
       f ~data_dir)
;;

let run_pong_game ~data_dir ~player_file mode =
  Pong.load_player ~file:player_file
  >>= fun player ->
  let module Pong_player = (val player : Pong_player_intf.S) in
  let module Args = (struct let mode = mode;; end) in
  let module Pong =
    (val (module Pong.Make(Pong_player)(Args) : Game.S) : Game.S)
  in
  Game.run (module Pong) ~data_dir
;;

let main () =
  Command.run
    (Command.group ~summary:"Spells and Roses"
       [ ( "test",
           Command.group ~summary:"System tests"
             [ ( "moving-rectangle",
                 test_command Tests.Moving_rectangle.run
                   ~summary:"Display a white rectangle moving horizontally" )
             ; ( "rectangles",
                 test_command Tests.Rectangles.run
                   ~summary:"Display a several static rectangles" )
             ; ( "text",
                 test_command Tests.Static_text.run
                   ~summary:"Display static text" )
             ; ( "dancing-banana",
                 test_command Tests.Dancing_banana.run
                   ~summary:"Display a dancing banana" )
             ; ( "psychedelic-cat",
                 test_command Tests.Psy_cat.run
                   ~summary:"Display a very special cat" )
             ; ( "image",
                 Command.basic
                   ~summary:"Write a checkerboard image"
                   Flag.(
                     empty
                     +> flag "device-gamma" (required float)
                          ~doc:"FLOAT gamma value to use when encoding image"
                     +> anon ("OUTPUT" %: file))
                   (fun device_gamma filename () ->
                      Image.save_ppm (Image.Test.checkerboard ())
                        ~filename ~device_gamma ()))
             ])
       ; ("script",
          Command.async_basic
            ~summary:"Run a script"
            Flag.
              ( empty
                +> flag "check-only" no_arg
                     ~doc:" Check that a script is loadable without running it"
                +> anon ("FILE" %: file)
                +> anon (sequence ("ARG" %: string)))
            (fun check_only file args () ->
               if check_only
               then Script.check_load ~file
               else Script.run ~file ~args))
       ; ("edit",
          Command.group ~summary:"Editor"
            [ ("world",
               Command.async_basic
                 ~summary:"Edit a world"
                 Flag.
                   ( empty
                     +> anon ("FILE" %: file)
                     +> data_dir )
                 (fun file data_dir () ->
                    World_editor.edit ~file ~data_dir))
            ])
       ; ("demo",
          Command.async_basic
            ~summary:"Run demo"
            Flag.(empty +> data_dir)
            (fun data_dir () ->
               Game.run (module Demo) ~data_dir))
       ; ("pong",
          Command.group
            ~summary:"Play pong"
            [ ("host",
               Command.async_basic
                 ~summary:"Host a pong game"
                 Flag.(empty +> data_dir +> player_file)
                 (fun data_dir player_file () ->
                    run_pong_game ~data_dir ~player_file `Host))
            ; ("connect-to",
               Command.async_basic
                 ~summary:"Connect to a pong game"
                 Flag.(empty +> data_dir +> player_file +> host)
                 (fun data_dir player_file host () ->
                    run_pong_game ~data_dir ~player_file (`Connect_to host)))
            ])
       ])
;;

let () = Exn.handle_uncaught ~exit:true main;;
