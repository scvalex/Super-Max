module Simple_query : sig
  type 'a t

  val create :
       event_map : (Sdlkeycode.t * 'a) list
    -> query : string
    -> width : int
    -> height : int
    -> font : string
    -> size_pt : int
    -> 'a t

  val on_event : 'a t -> Sdlevent.t -> 'a option

  val to_drawing : 'a t -> Drawing.t
end
