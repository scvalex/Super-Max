open Core.Std
open Async.Std

module Glog = Async.Std.Log.Global

module Tag = struct
  module T = struct
    type t =
      [ `Input
      | `Renderer
      ] with sexp, compare
  end

  include T
  include Comparable.Make(T)
  include Sexpable.To_stringable(T)
end

module Debug_tags = struct
  type t = Tag.Set.t with sexp
end

let debug_tags =
  match Core.Std.Sys.getenv "DEBUG" with
  | None      -> Tag.Set.empty
  | Some tags -> Debug_tags.t_of_sexp (Sexp.of_string ("(" ^ tags ^ ")"))
;;

(** Thread-safe logging: We want to be able to log from any thread, be
    it Async or otherwise.  However, we don't trust Async.Log to be
    thread-safe.  So, we write messages to a thread-safe queue, then
    drain them periodically into Async.Log. *)
let log_queue = ref None;;

let get_or_create_log_queue () =
  match !log_queue with
  | Some queue ->
    queue
  | None ->
    let queue = Squeue.create 16 in
    let queue_to_log () =
      let linked_queue = Linked_queue.create () in
      Squeue.transfer_queue_nowait queue linked_queue;
      Linked_queue.iter linked_queue ~f:(fun (time, str) ->
        Glog.info ~time "%s" str)
    in
    Shutdown.at_shutdown (fun () ->
      queue_to_log ();
      Glog.flushed ());
    Clock.every (sec 0.1) queue_to_log;
    log_queue := Some queue;
    queue
;;

let log tag fmt =
  ksprintf (fun str ->
    if Set.mem debug_tags tag then begin
      let log_queue = get_or_create_log_queue () in
      Squeue.push_uncond log_queue (Time.now (), str)
    end) fmt
;;
