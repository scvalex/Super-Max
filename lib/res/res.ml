open Core.Std
open Async.Std

module Metadata = struct
  type t = {
    n_vertices    : int;
    source        : string option;
    source_id     : string option;
    creation_time : Time.t;
  } with fields, sexp, bin_io

  let create = Fields.create;;
end

module Mesh = struct
  type t = {
    vertices : Float_array.t;
  } with fields

  let create = Fields.create;;
end

type t = {
  metadata : Metadata.t;
  data     : [`Mesh of Mesh.t];
}

let create_mesh ?source ?source_id ~vertices () =
  let metadata =
    Metadata.create ~source ~source_id ~creation_time:(Time.now ())
      ~n_vertices:(Float_array.length vertices)
  in
  let data = `Mesh (Mesh.create ~vertices) in
  { metadata; data; }
;;

let metadata t =
  Sexp.to_string_mach (Metadata.sexp_of_t t.metadata)
;;

(* CR scvalex: Serialize these as a stream of atoms. *)
module On_disk = struct
  module Mesh = struct
    type t = {
      vertices : float array;
    } with fields, bin_io

    let create = Fields.create;;
  end

  type t = {
    metadata : Metadata.t;
    data     : [`Mesh of Mesh.t];
  } with fields, bin_io

  let create = Fields.create;;
end

let load file =
  Deferred.Or_error.try_with (fun () ->
    Reader.with_file file ~f:(fun reader ->
      Reader.read_bin_prot reader On_disk.bin_reader_t)
    >>| function
    | `Eof ->
      failwithf "ran out of input reading %s" file ()
    | `Ok on_disk ->
      let metadata = On_disk.metadata on_disk in
      let data =
        match On_disk.data on_disk with
        | `Mesh mesh ->
          `Mesh (Mesh.create ~vertices:(Float_array.of_array (On_disk.Mesh.vertices mesh)))
      in
      { metadata; data; })
;;

let save t file =
  Deferred.Or_error.try_with (fun () ->
    Writer.with_file file ~f:(fun writer ->
      let data =
        match t.data with
        | `Mesh mesh ->
          let vertices = Mesh.vertices mesh in
          let vertices' = Array.create ~len:(Float_array.length vertices) 0.0 in
          for idx = 0 to Float_array.length vertices - 1 do
            Array.set vertices' idx vertices.{idx}
          done;
          `Mesh (On_disk.Mesh.create ~vertices:vertices')
      in
      let on_disk = On_disk.create ~metadata:t.metadata ~data in
      Writer.write_bin_prot writer On_disk.bin_writer_t on_disk;
      Writer.flushed writer))
;;
