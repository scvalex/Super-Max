open Linear.Std

val ray_trace :
  image : Image.t
  -> scene : Scene.t
  -> camera : Camera.t
  -> x0 : int
  -> x1 : int
  -> y0 : int
  -> y1 : int
  -> unit
