open Core.Std
open Types

let compute_eye_ray ~x ~y ~width ~height ~camera =
  let aspect = height /. width in
  let s = (0.0 -. 2.0) *. tan (Camera.field_of_view_x camera *. 0.5) in
  let start =
    Vector3.scale
      (Vector3.create
         ~x:((x /. width -. 0.5) *. s)
         ~y:((0.0 -. (y /. height -. 0.5)) *. s *. aspect)
         ~z:1.0)
      (Camera.z_near camera)
  in
  Ray.create
    ~origin:(Point3.of_vector start)
    ~direction:(Vector3.direction start)
;;

let intersect ~ray ~triangle =
  let (t1, t2, t3) = Triangle.vertices triangle in
  let (t1, t2, t3) = Point3.(to_vector t1, to_vector t2, to_vector t3) in
  let e1 = Vector3.(t2 - t1) in
  let e2 = Vector3.(t3 - t1) in
  let q = Vector3.cross (Ray.direction ray) e2 in
  let a = Vector3.dot e1 q in
  let s = Vector3.(Point3.to_vector (Ray.origin ray) - t1) in
  let r = Vector3.cross s e1 in
  let w2 = Vector3.(dot s q) /. a in
  let w3 = Vector3.(dot (Ray.direction ray) r) /. a in
  let w1 = 1.0 -. (w2 +. w3) in
  let distance = Vector3.(dot e2 r) /. a in
  let epsilon = 1e-7 in
  let epsilon2 = 1e-10 in
  if Float.(a <= epsilon
            || w1 < -epsilon2 || w2 < -epsilon2 || w3 < -epsilon2
            || distance <= 0.0)
  then (Float.infinity, (w1, w2, w3))
  else (distance, (w1, w2, w3))
;;

let sample_ray_triangle ~scene ~ray ~triangle ~distance =
  let (distance', (w1, w2, w3)) = intersect ~ray ~triangle in
  if Float.(distance' >= distance)
  then begin
    None
  end else begin
    let intersection_point =
      Point3.add_vector
        (Ray.origin ray)
        (Vector3.scale (Ray.direction ray) distance')
    in
    let normal =
      let (n1, n2, n3) = Triangle.normals triangle in
      Vector3.(direction (scale n1 w1 + scale n2 w2 + scale n3 w3))
    in
    let w_o =
      Vector3.scale (Ray.direction ray) (-1.0)
    in
    (* let radiance = *)
    (*   shade ~scene ~triangle ~intersection_point ~normal ~w_o *)
    (* in *)
    let radiance =
      let _ = scene in
      let _ = normal in
      let _ = intersection_point in
      let _ = w_o in
      Radiance3.of_color (Color3.create ~r:1.0 ~g:1.0 ~b:1.0)
    in
    Some (radiance, distance')
  end
;;

let ray_trace ~image ~scene ~camera ~x0 ~x1 ~y0 ~y1 =
  let width = Float.of_int (Image.width image) in
  let height = Float.of_int (Image.height image) in
  for y = y0 to y1 - 1 do
    for x = x0 to x1 - 1 do
      let eye_ray =
        compute_eye_ray ~width ~height ~camera
          ~x:(Float.of_int x +. 0.5)
          ~y:(Float.of_int y +. 0.5)
      in
      let (_ : float) =
        Set.fold_right (Scene.triangles scene) ~init:Float.infinity
          ~f:(fun triangle distance ->
            match sample_ray_triangle ~scene ~ray:eye_ray ~triangle ~distance with
            | Some (radiance, distance) ->
              Image.set image ~x ~y radiance;
              distance
            | None ->
              distance)
      in
      ()
    done
  done
;;
