open Core.Std let _ = _squelch_unused_module_warning_
open Linear.Std

module A2 = Bigarray.Array2

type t = {
  width : int;
  height : int;
  data : (float, Bigarray.float64_elt, Bigarray.c_layout) A2.t;
} with fields

let create ~width ~height () =
  let data = A2.create Bigarray.float64 Bigarray.c_layout height (3 * width) in
  { width; height; data; }
;;

let set t ~x ~y rad3 =
  A2.set t.data y (x * 3)     (Radiance3.r rad3);
  A2.set t.data y (x * 3 + 1) (Radiance3.g rad3);
  A2.set t.data y (x * 3 + 2) (Radiance3.b rad3);
;;

let get t ~x ~y =
  Radiance3.create
    ~r:(A2.get t.data y (x * 3))
    ~g:(A2.get t.data y (x * 3 + 1))
    ~b:(A2.get t.data y (x * 3 + 2))
;;

let save_ppm t ~filename ?(device_gamma = 2.2) ?(display_constant = 1.0) () =
  let ppm_encode_gamma radiance =
    Float.(iround_exn ((min 1.0 (max 0.0 (radiance *. display_constant)))
                       ** (1.0 /. device_gamma)
                       * 255.0))
  in
  Out_channel.with_file filename ~f:(fun out ->
    let print fmt =
      ksprintf (fun str ->
        Out_channel.output_string out str;
        Out_channel.newline out) fmt
    in
    print "P3 %d %d 255" t.width t.height;
    for y = 0 to t.height - 1 do
      print "\n# y = %d" y;
      for x = 0 to t.width - 1 do
        let rad3 = get t ~x ~y in
        print "%d %d %d"
          (ppm_encode_gamma (Radiance3.r rad3))
          (ppm_encode_gamma (Radiance3.g rad3))
          (ppm_encode_gamma (Radiance3.b rad3));
      done
    done)
;;

module Test = struct
  let checkerboard () =
    let width = 400 in
    let height = 300 in
    let t = create ~width ~height () in
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        let rad3 =
          if Int.((x + y) % 2 = 0)
          then
            let p = Float.(of_int y /. of_int height) in
            Radiance3.create ~r:p ~g:p ~b:p
          else
            Radiance3.create ~r:0.0 ~g:0.0 ~b:0.1
        in
        set t ~x ~y rad3
      done
    done;
    t
  ;;
end
