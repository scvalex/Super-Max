include module type of World_intf

val to_drawing :
     ((_, _) Entity.t * Position.t) Entity.Id.Map.t
  -> layers : string list
  -> camera : ([`X of float] * [`Y of float])
  -> Drawing.t
