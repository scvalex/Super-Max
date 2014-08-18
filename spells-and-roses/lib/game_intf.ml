open Core.Std

module type Binable_and_sexpable = sig
  type t

  include Binable.S with type t := t
  include Sexpable.S with type t := t
end

module Client_id =
  String_id.Make(struct let module_name = "Client_id" end)

module Query_response = struct
  type 's t =
    [ `Accept of 's
    | `Reject of string
    ] with sexp
end

module type S = sig
  module Update : sig
    module Query : sig
      type t

      include Binable_and_sexpable with type t := t
    end

    module Snapshot : sig
      type t

      include Binable_and_sexpable with type t := t
    end

    (** An [Update.t] is sent from [Node.t] to [Node.t] over the
        network.  It is the only mechanism to propagate events from
        one instance of the game to another. *)
    type t

    include Binable_and_sexpable with type t := t
  end

  type t

  val create :
       width : int
    -> height : int
    -> t

  val on_event :
       t
    -> engine : Update.t Engine.t
    -> Sdlevent.t
    -> t

  val on_step :
       t
    -> engine : Update.t Engine.t
    -> t

  val on_update_query :
       t
    -> engine : Update.t Engine.t
    -> client_id : Client_id.t
    -> Update.Query.t
    -> (t * Update.Snapshot.t Query_response.t)

  val on_update :
       t
    -> engine : Update.t Engine.t
    -> client_id : Client_id.t
    -> Update.t
    -> t

  val to_drawing : t -> Drawing.t

  val steps_per_second : float
end
