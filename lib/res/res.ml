open Core.Std
open Async.Std

module Metadata = struct
  type t = {
    positions_count : int option;
    indices_length   : int option;
    source           : string option;
    source_id        : string option;
    creation_time    : Time.t;
  } with fields, sexp, bin_io

  let create ?positions_count ?indices_length ?source ?source_id () =
    let creation_time = Time.now () in
    { positions_count; indices_length; source; source_id; creation_time; }
  ;;
end

module Mesh = struct
  type t = {
    id        : Res_id.t;
    positions : Float_array.t;
    indices   : Int_array.t;
  } with fields

  let create = Fields.create;;
end

module Program = struct
  type t = {
    id       : Res_id.t;
    vertex   : string;
    fragment : string;
  } with fields

  let create = Fields.create;;

  let with_vertex t vertex =
    { t with vertex; }
  ;;

  let with_fragment t fragment =
    { t with fragment; }
  ;;
end

type t = {
  metadata : Metadata.t;
  data     : [`Mesh of Mesh.t | `Program of Program.t];
} with fields

let create_mesh ?source ?source_id ~positions ~indices id =
  if Int.(Float_array.length positions mod 3 <> 0) then
    failwithf "positions length not a multiple of 3" ();
  let metadata =
    Metadata.create ?source ?source_id ()
      ~positions_count:(Float_array.length positions / 3)
      ~indices_length:(Int_array.length indices)
  in
  let data = `Mesh (Mesh.create ~positions ~indices ~id) in
  { metadata; data; }
;;

let create_program ~vertex ~fragment id =
  let metadata = Metadata.create () in
  let data = `Program (Program.create ~vertex ~fragment ~id) in
  { metadata; data; }
;;

let metadata t =
  Sexp.to_string_mach (Metadata.sexp_of_t t.metadata)
;;

let id t =
  match t.data with
  | `Mesh mesh       -> Mesh.id mesh
  | `Program program -> Program.id program
;;

module Chunk = struct
  type t =
    | Metadata of [`Mesh | `Program] * Metadata.t
    | Positions of float array
    | Indices of int32 array
    | Source_code of [`Vertex | `Fragment] * string
  with bin_io
end

let load ~id file =
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
          let positions_count =
            Option.value_exn ~here:_here_
              (Metadata.positions_count metadata)
          in
          let indices_length =
            Option.value_exn ~here:_here_
              (Metadata.indices_length metadata)
          in
          let positions = Float_array.create (3 * positions_count) in
          let indices = Int_array.create indices_length in
          let data = `Mesh (Mesh.create ~positions ~indices ~id) in
          Some ({ metadata; data; }, 0, 0)
        | (None, Chunk.Metadata (`Program, metadata)) ->
          let data = `Program (Program.create ~vertex:"" ~fragment:"" ~id) in
          Some ({ metadata; data; }, 0, 0)
        | (None, _) ->
          failwithf "Metadata chunk was not first in %s" file ()
        | (Some _, Chunk.Metadata _) ->
          failwithf "multiple Metadata chunks in %s" file ()
        | (Some (t, next_position, next_index), Chunk.Positions positions) -> begin
            match t.data with
            | `Mesh mesh ->
              for idx = 0 to Array.length positions - 1 do
                (Mesh.positions mesh).{next_position + idx} <- positions.(idx)
              done;
              Some (t, next_position + Array.length positions, next_index)
            | _ ->
              failwithf "Positions chunk in non-mesh %s" file ()
        end
        | (Some (t, next_position, next_index), Chunk.Indices indices) -> begin
            match t.data with
            | `Mesh mesh ->
              for idx = 0 to Array.length indices - 1 do
                (Mesh.indices mesh).{next_index + idx} <- indices.(idx)
              done;
              Some (t, next_position, next_index + Array.length indices)
            | _ ->
              failwithf "Indices chunk in non-mesh %s" file ()
          end
        | (Some (t, next_position, next_index), Chunk.Source_code (kind, code)) -> begin
            match t.data with
            | `Program program ->
              let data =
                match kind with
                | `Vertex   -> `Program (Program.with_vertex program code)
                | `Fragment -> `Program (Program.with_fragment program code)
              in
              Some ({ t with data; }, next_position, next_index)
            | _ ->
              failwithf "Source_code chunk in non-program %s" file ()
          end)
      >>= function
      | None ->
        failwithf "nothing was read from %s" file ()
      | Some (t, _, _) ->
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
      let write_chunk chunk =
        Writer.write_bin_prot writer Chunk.bin_writer_t chunk;
        Writer.flushed writer
      in
      match t.data with
      | `Mesh mesh ->
        write_chunk (Chunk.Metadata (`Mesh, t.metadata))
        >>= fun () ->
        let rec loop ~array ~length ~pack ~dummy idx =
          if Int.(idx >= length)
          then begin
            Deferred.unit
          end else begin
            let count = Int.min 1000 Int.(length - idx) in
            let buf = Array.create ~len:count dummy in
            for jdx = 0 to count - 1 do
              buf.(jdx) <- array.{idx + jdx}
            done;
            write_chunk (pack buf)
            >>= fun () ->
            loop ~array ~length ~pack ~dummy (idx + count)
          end
        in
        loop 0
          ~array:(Mesh.positions mesh)
          ~length:(Float_array.length (Mesh.positions mesh))
          ~pack:(fun buf -> Chunk.Positions buf)
          ~dummy:0.0
        >>= fun () ->
        loop 0
          ~array:(Mesh.indices mesh)
          ~length:(Int_array.length (Mesh.indices mesh))
          ~pack:(fun buf -> Chunk.Indices buf)
          ~dummy:(Int32.of_int_exn 0)
      | `Program program ->
        write_chunk (Chunk.Metadata (`Program, t.metadata))
        >>= fun () ->
        write_chunk (Chunk.Source_code (`Vertex, Program.vertex program))
        >>= fun () ->
        write_chunk (Chunk.Source_code (`Fragment, Program.fragment program))))
;;
