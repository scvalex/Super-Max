open Core.Std
open Async.Std
open Super_max.Std

module Flag = struct
  include Command.Spec

  let data_dir =
    flag "data-dir" (optional_with_default "resources" file)
      ~doc:"DIR location of game resources"
  ;;
end

let test_command ~summary f =
  Command.async_basic ~summary
    Flag.(empty +> data_dir)
    (fun data_dir () ->
       f ~data_dir)
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
             ])
       ; ("script",
          Command.async_basic
            ~summary:"Run a script"
            Flag.
              ( empty
                +> flag "check-only" no_arg
                  ~doc:" Check that a script is loadable without running it"
                +> anon ("FILE" %: file) )
            (fun check_only file () ->
               if check_only
               then Script.check_load ~file
               else Script.run ~file))
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
       ; ("demo",
          Command.async_basic
            ~summary:"Run demo"
            Flag.(empty +> data_dir)
            (fun data_dir () ->
               Game.run (module Demo) ~data_dir))
       ])
;;

let () = Exn.handle_uncaught ~exit:true main;;
