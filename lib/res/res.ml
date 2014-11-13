open Core.Std
open Async.Std

module Mesh = struct
  type t = {
    vertices : Float_array.t;
  } with fields

  let create = Fields.create;;
end

type t =
  | Mesh of Mesh.t

(* CR scvalex: Serialize these as a stream of atoms. *)
module On_disk = struct
  module Mesh = struct
    type t = {
      vertices : float array;
    } with fields, bin_io

    let create = Fields.create;;
  end

  type t =
    | Mesh of Mesh.t
  with bin_io
end

let load file =
  Deferred.Or_error.try_with (fun () ->
    Reader.with_file file ~f:(fun reader ->
      Reader.read_bin_prot reader On_disk.bin_reader_t)
    >>| function
    | `Eof ->
      failwithf "ran out of input reading %s" file ()
    | `Ok (On_disk.Mesh mesh) ->
      Mesh (Mesh.create ~vertices:(Float_array.of_array (On_disk.Mesh.vertices mesh))))
;;

let save t file =
  Deferred.Or_error.try_with (fun () ->
    Writer.with_file file ~f:(fun writer ->
      let on_disk =
        match t with
        | Mesh mesh ->
          let vertices = Mesh.vertices mesh in
          let vertices' = Array.create ~len:(Float_array.length vertices) 0.0 in
          for idx = 0 to Float_array.length vertices - 1 do
            Array.set vertices' idx vertices.{idx}
          done;
          On_disk.Mesh (On_disk.Mesh.create ~vertices:vertices')
      in
      Writer.write_bin_prot writer On_disk.bin_writer_t on_disk;
      Writer.flushed writer))
;;
