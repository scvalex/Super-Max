type t

val create :
     width : float
  -> height : float
  -> t

val to_drawing : t -> Drawing.t
