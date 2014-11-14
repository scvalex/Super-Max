open Core.Std
open Async.Std

module Metadata = struct
  type t = {
    vertex_count  : int;
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
  if Int.(Float_array.length vertices mod 3 <> 0) then
    failwithf "vertices length not a multiple of 3" ();
  let metadata =
    Metadata.create ~source ~source_id ~creation_time:(Time.now ())
      ~vertex_count:(Float_array.length vertices / 3)
  in
  let data = `Mesh (Mesh.create ~vertices) in
  { metadata; data; }
;;

let metadata t =
  Sexp.to_string_mach (Metadata.sexp_of_t t.metadata)
;;

module Chunk = struct
  type t =
    | Metadata of [`Mesh] * Metadata.t
    | Vertices of float array
  with bin_io
end

let load file =
  Deferred.Or_error.try_with (fun () ->
    Reader.with_file file ~f:(fun reader ->
      let (chunks, result) =
        Unpack_sequence.unpack_bin_prot_from_reader Chunk.bin_reader_t reader
      in
      Pipe.fold ~init:None chunks ~f:(fun acc chunk ->
        Scheduler.yield ()
        >>| fun () ->
        match (acc, chunk) with
        | (None, Chunk.Metadata (`Mesh, metadata)) ->
          let vertices = Float_array.create (3 * Metadata.vertex_count metadata) in
          let data = `Mesh (Mesh.create ~vertices) in
          Some ({ metadata; data; }, 0)
        | (None, _) ->
          failwithf "Metadata chunk was not first in %s" file ()
        | (Some _, Chunk.Metadata _) ->
          failwithf "multiple Metadata chunks in %s" file ()
        | (Some (t, next_vertex), Chunk.Vertices vertices) ->
          match t.data with
          | `Mesh mesh ->
            for idx = 0 to Array.length vertices - 1 do
              (Mesh.vertices mesh).{next_vertex + idx} <- vertices.(idx)
            done;
            Some (t, next_vertex + Array.length vertices))
      >>= function
      | None ->
        failwithf "nothing was read from %s" file ()
      | Some (t, _) ->
        result
        >>| fun result ->
        match result with
        | Unpack_sequence.Result.Input_closed ->
          t
        | _ ->
          Error.raise (Unpack_sequence.Result.to_error result)))
;;

let save t file =
  Deferred.Or_error.try_with (fun () ->
    Writer.with_file file ~f:(fun writer ->
      match t.data with
      | `Mesh mesh ->
        let write_chunk chunk =
          Writer.write_bin_prot writer Chunk.bin_writer_t chunk;
          Writer.flushed writer
        in
        write_chunk (Chunk.Metadata (`Mesh, t.metadata))
        >>= fun () ->
        let vertices = Mesh.vertices mesh in
        let length = Float_array.length vertices in
        let rec loop idx =
          if Int.(idx >= length)
          then begin
            Deferred.unit
          end else begin
            let count = Int.min 1000 Int.(length - idx) in
            let buf = Array.create ~len:count 0.0 in
            for jdx = 0 to count - 1 do
              buf.(jdx) <- vertices.{idx + jdx}
            done;
            write_chunk (Chunk.Vertices buf)
            >>= fun () ->
            loop (idx + count)
          end
        in
        loop 0))
;;
