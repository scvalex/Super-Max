open Core.Std
open Async.Std

type xy = {
  x : float;
  y : float;
} with sexp

type rgba = {
  red   : float;
  green : float;
  blue  : float;
  alpha : float;
} with sexp

type text_position = ([`X of [`Left | `Centre | `Right]]
                      * [`Y of [`Top | `Centre | `Bottom]]) with sexp
type text = {
  str      : string list;
  font     : string;
  size_pt  : int;
  position : text_position;
} with sexp

type image_clip = ([`X of int] * [`Y of int]
                   * [`Width of int] * [`Height of int]) with sexp

type image = {
  image     : string;
  clip      : image_clip option;
  angle_deg : float option;
} with sexp

type texture_with_size = Sdltexture.t * [`Width of int] * [`Height of int]

type rectangle = {
  width  : float;
  height : float;
  filled : bool;
} with sexp

let rgb_a_of_colour rgba =
  let rgb =
    (Float.iround_exn (rgba.red *. 255.0),
     Float.iround_exn (rgba.green *. 255.0),
     Float.iround_exn (rgba.blue *. 255.0))
  in
  let a = Float.iround_exn (rgba.alpha *. 255.0) in
  (rgb, a)
;;

let rgba_of_colour rgba =
  (Float.iround_exn (rgba.red *. 255.0),
   Float.iround_exn (rgba.green *. 255.0),
   Float.iround_exn (rgba.blue *. 255.0),
   Float.iround_exn (rgba.alpha *. 255.0))
;;

module Trans : sig
  type t

  include Sexpable.S with type t := t

  val id : t
  val scale : t -> xy -> t
  val translate : t -> xy -> t
  val apply : t -> xy -> xy
end = struct
  type t = Mat.mat3 with sexp

  let id = Mat.id ();;

  let scale t {x; y} =
    Mat.(t * scale ~x ~y)
  ;;

  let translate t {x; y} =
    Mat.(t * translate ~x ~y)
  ;;

  let apply t {x; y} =
    let (x, y) = Mat.(xy_of_vec3 (t *| vec3_of_xy ~x ~y)) in
    {x; y}
  ;;
end

module Context : sig
  type t

  val create :
       renderer : Sdlrender.t
    -> thread : In_thread.Helper_thread.t
    -> data_dir : string
    -> t

  val renderer : t -> Sdlrender.t

  val thread : t -> In_thread.Helper_thread.t

  val get_font :
       t
    -> string
    -> int
    -> Sdlttf.font

  val get_or_create_texture :
       t
    -> create : (data_dir : string -> texture_with_size)
    -> string
    -> texture_with_size

  val stats : t -> string
end = struct
  module Font_and_size = struct
    module T = struct
      type t = (string * int) with sexp, compare

      let hash (font, size) =
        String.hash (sprintf "%s-%d" font size)
      ;;
    end

    include T
    include Hashable.Make(T)
  end

  type t = {
    renderer : Sdlrender.t;
    fonts    : Sdlttf.font Font_and_size.Table.t;
    textures : texture_with_size String.Table.t;
    thread   : In_thread.Helper_thread.t;
    data_dir : string;
  } with fields

  let create ~renderer ~thread ~data_dir =
    let fonts = Font_and_size.Table.create () in
    let textures = String.Table.create () in
    { renderer; fonts; textures; thread; data_dir; }
  ;;

  let get_font t font_name size_pt =
    Hashtbl.find_or_add t.fonts (font_name, size_pt)
      ~default:(fun () ->
          Sdlttf.open_font ~file:(t.data_dir ^/ font_name) ~ptsize:size_pt)
  ;;

  let get_or_create_texture t ~create id =
    Hashtbl.find_or_add t.textures id
      ~default:(fun () -> create ~data_dir:t.data_dir)
  ;;

  let stats t =
    sprintf "Drawing stats:\n - fonts: %d\n - textures: %d"
      (Hashtbl.length t.fonts) (Hashtbl.length t.textures)
  ;;
end

type t =
  | Empty
  | Translate of (xy * t)
  | Scale of (xy * t)
  | Rectangle of rectangle
  | Colour of (rgba * t)
  | Many of t list
  | Text of text
  | Image of image
with sexp

let empty = Empty;;

let translate ~x ~y t =
  Translate ({x; y;}, t)
;;

let scale ~x ~y t =
  if Float.(x < 0.0 || y < 0.0) then
    failwithf "Cannot scale by negative amounts: (%f, %f)" x y ();
  Scale ({x; y;}, t)
;;

let rectangle ~width ~height ~filled =
  if Float.(width < 0.0 || height < 0.0) then
    failwithf "Rectangle dimensions must be positive: (%f, %f)"
      width height ();
  Rectangle {width; height; filled;}
;;

let colour ~r:red ~g:green ~b:blue ?a:(alpha = 1.0) t =
  if Float.(red < 0.0 || 1.0 < red
            || green < 0.0 || 1.0 < green
            || blue < 0.0 || 1.0 < blue
            || alpha < 0.0 || 1.0 < alpha)
  then
    failwithf "Invalid colour: (%f, %f, %f, %f)"
      red green blue alpha ();
  Colour ({red; green; blue; alpha;}, t)
;;

let many ts =
  Many ts
;;

let text ~font ~size_pt ?(position = (`X `Left, `Y `Top)) str =
  Text { font; size_pt; str; position; }
;;

let image ?clip ?angle_deg image =
  Image { image; clip; angle_deg; }
;;

let centered_normalized_scene ~width ~height t =
  let width = Float.of_int width in
  let height = Float.of_int height in
  let dim = Float.min width height in
  translate
    ~x:((width -. dim) /. 2.0)
    ~y:((height -. dim) /. 2.0)
    (scale ~x:dim ~y:dim t)
;;

let render_rectangle ~ctx ~trans ~colour ~width ~height ~filled =
  let xy0 = Trans.apply trans {x = 0.0; y = 0.0;} in
  let xy1 = Trans.apply trans {x = width; y = height;} in
  let (rgb, a) = rgb_a_of_colour colour in
  Sdlrender.set_draw_color (Context.renderer ctx) ~rgb ~a;
  let rect =
    Sdlrect.make4
      ~x:(Float.iround_exn xy0.x)
      ~y:(Float.iround_exn xy0.y)
      ~w:(Float.iround_exn (xy1.x -. xy0.x))
      ~h:(Float.iround_exn (xy1.y -. xy0.y))
  in
  if filled
  then begin
    Sdlrender.fill_rect (Context.renderer ctx) rect
  end else begin
    Sdlrender.draw_rect (Context.renderer ctx) rect;
    let rect =
      Sdlrect.make4
        ~x:(rect.Sdlrect.x + 1)
        ~y:(rect.Sdlrect.y + 1)
        ~w:(rect.Sdlrect.w - 2)
        ~h:(rect.Sdlrect.h - 2)
    in
    Sdlrender.draw_rect (Context.renderer ctx) rect
  end
;;

let render_text ~ctx ~trans ~colour text =
  let line_textures =
    List.map text.str ~f:(fun str ->
      Context.get_or_create_texture ctx
        (sprintf "text-%s-%d-%s" text.font text.size_pt str)
        ~create:(fun ~data_dir:_ ->
          let font = Context.get_font ctx text.font text.size_pt in
          let (r, g, b, a) = rgba_of_colour colour in
          let color = {Sdlttf. r; g; b; a} in
          let surface =
            Sdlttf.render_text_solid font ~text:str ~color
          in
          let w = Sdlsurface.get_width surface in
          let h = Sdlsurface.get_height surface in
          let texture =
            Sdltexture.create_from_surface (Context.renderer ctx) surface
          in
          Sdlsurface.free surface;
          (texture, `Width w, `Height h)))
  in
  let xy = Trans.apply trans {x = 0.0; y = 0.0;} in
  let h_and_a_half h =
    h + Float.(iround_exn (0.5 *. of_int h))
  in
  let (max_w, total_h) =
    List.fold_left ~init:(0, 0) line_textures
      ~f:(fun (max_w, total_h) (_, `Width w, `Height h) ->
        (Int.max max_w w, total_h + h_and_a_half h))
  in
  let (x, y) =
    let x =
      let x = Float.iround_exn xy.x in
      match text.position with
      | (`X `Left, _)   -> x
      | (`X `Centre, _) -> x - max_w / 2
      | (`X `Right, _)  -> x - max_w
    in
    let y =
      let y = Float.iround_exn xy.y in
      match text.position with
      | (_, `Y `Top)    -> y
      | (_, `Y `Centre) -> y - total_h / 2
      | (_, `Y `Bottom) -> y - total_h
    in
    (x, y)
  in
  let (_ : (int * int)) =
    List.fold_left ~init:(x, y) line_textures
      ~f:(fun (x, y) (texture, `Width w, `Height h) ->
        let src_rect = Sdlrect.make4 ~x:0 ~y:0 ~w ~h in
        let dst_rect = Sdlrect.make4 ~x ~y ~w ~h in
        Sdlrender.copy (Context.renderer ctx) ~texture ~src_rect ~dst_rect ();
        (x, y + h_and_a_half h))
  in
  ()
;;

let render_image ~ctx ~trans ~colour:_ image =
  let (texture, `Width width, `Height height) =
    Context.get_or_create_texture ctx
      (sprintf "image-%s" image.image)
      ~create:(fun ~data_dir ->
        let rwop =
          Sdlrwops.from_file ~mode:"rb"
            ~filename:(data_dir ^/ image.image)
        in
        let surface =
          match Filename.split_extension image.image with
          | (_, Some "png") -> Sdlimage.load_png_rw rwop
          | (_, Some "jpg") -> Sdlimage.load_jpg_rw rwop
          | _               -> failwithf "Unknown format: %s" image.image ()
        in
        Sdlrwops.close rwop;
        let w = Sdlsurface.get_width surface in
        let h = Sdlsurface.get_height surface in
        let texture =
          Sdltexture.create_from_surface (Context.renderer ctx) surface
        in
        Sdlsurface.free surface;
        (texture, `Width w, `Height h))
  in
  let src_rect =
    match image.clip with
    | None ->
      Sdlrect.make4 ~x:0 ~y:0 ~w:width ~h:height
    | Some (`X x, `Y y, `Width width, `Height height) ->
      Sdlrect.make4 ~x ~y ~w:width ~h:height
  in
  let xy0 = Trans.apply trans {x = 0.0; y = 0.0;} in
  let xy1 =
    Trans.apply trans { x = Float.of_int src_rect.Sdlrect.w;
                        y = Float.of_int src_rect.Sdlrect.h; }
  in
  let dst_rect =
    Sdlrect.make4
      ~x:(Float.iround_exn xy0.x)
      ~y:(Float.iround_exn xy0.y)
      ~w:(Float.iround_exn (xy1.x -. xy0.x))
      ~h:(Float.iround_exn (xy1.y -. xy0.y))
  in
  match image.angle_deg with
  | None ->
    Sdlrender.copy (Context.renderer ctx) ~texture ~src_rect ~dst_rect ()
  | Some angle ->
    Sdlrender.copyEx (Context.renderer ctx) ~texture ~src_rect ~dst_rect ~angle ()
;;

let render_blocking t ~ctx =
  let rec loop trans colour = function
    | Empty ->
      ()
    | Translate (xy, t) ->
      let trans = Trans.translate trans xy in
      loop trans colour t
    | Scale (xy, t) ->
      let trans = Trans.scale trans xy in
      loop trans colour t
    | Rectangle {width; height; filled;} ->
      render_rectangle ~ctx ~trans ~colour ~width ~height ~filled
    | Colour (colour, t) ->
      loop trans colour t
    | Many ts ->
      List.iter ts ~f:(loop trans colour)
    | Text text ->
      render_text ~ctx ~trans ~colour text
    | Image image ->
      render_image ~ctx ~trans ~colour image
  in
  Sdlrender.set_draw_color (Context.renderer ctx) ~rgb:(0, 0, 0) ~a:255;
  Sdlrender.clear (Context.renderer ctx);
  let grey = { red = 0.8; green = 0.8; blue = 0.8; alpha = 1.0; } in
  loop Trans.id grey t;
  Sdlrender.render_present (Context.renderer ctx)
;;

let render t ~ctx =
  In_thread.run (fun () -> render_blocking t ~ctx)
    ~thread:(Context.thread ctx)
;;

module Example = struct
  let rectangles ~width ~height =
    let half_square = rectangle ~width:0.48 ~height:0.48 ~filled:true in
    let rectangles =
      many
        [ translate ~x:0.01 ~y:0.01
            (colour ~r:1.0 ~g:0.0 ~b:0.0
               half_square)
        ; translate ~x:0.51 ~y:0.01
            (colour ~r:0.7 ~g:0.0 ~b:0.7
               half_square)
        ; translate ~x:0.01 ~y:0.51
            (colour ~r:0.7 ~g:0.0 ~b:0.7
               half_square)
        ; translate ~x:0.51 ~y:0.51
            (colour ~r:0.0 ~g:0.0 ~b:1.0
               half_square)
        ; translate ~x:0.4 ~y:0.1
            (colour ~r:0.0 ~g:0.5 ~b:0.0
               (rectangle ~width:0.59 ~height:0.3 ~filled:true))
        ]
    in
    centered_normalized_scene rectangles ~width ~height
  ;;
end
