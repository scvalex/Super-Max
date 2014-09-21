open Core.Std
open Linear.Std

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

let visible ~scene ~intersection_point ~w_i ~distance_to_light =
  let ray_bump_epsilon = 1e-4 in
  let shadow_ray =
    Ray.create
      ~origin:Point3.(add_vector intersection_point Vector3.(scale w_i ray_bump_epsilon))
      ~direction:w_i
  in
  let distance_to_light = distance_to_light -. ray_bump_epsilon in
  let obscured =
    Set.exists (Scene.triangles scene) ~f:(fun triangle ->
      let (distance_to_triangle, _) = intersect ~ray:shadow_ray ~triangle in
      Float.(distance_to_triangle < distance_to_light))
  in
  not obscured
;;

let shade ~scene ~triangle ~intersection_point ~normal ~w_o =
  Set.fold_right (Scene.lights scene) ~init:(Radiance3.create ~r:0.0 ~g:0.0 ~b:0.0)
    ~f:(fun light l_o ->
      let offset =
        Vector3.(Point3.to_vector (Light.position light)
                 - Point3.to_vector intersection_point)
      in
      let distance_to_light = Vector3.magnitude offset in
      let w_i = Vector3.scale offset (1.0 /. distance_to_light) in
      if visible ~scene ~intersection_point ~w_i ~distance_to_light
      then begin
        let l_i =
          Power3.to_radiance
            (Power3.scale (Light.power light)
               (1.0 /. (4.0 *. m_pi *. distance_to_light *. distance_to_light)))
        in
        let scattered =
          Bsdf.evaluate_finite_scattering_density (Triangle.bsdf triangle) ~w_i ~w_o
        in
        Radiance3.(l_o + l_i * scale scattered (Float.max 0.0 (Vector3.dot w_i normal)))
      end else begin
        l_o
      end)
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
    let w_o = Vector3.scale (Ray.direction ray) (-1.0) in
    let radiance =
      shade ~scene ~triangle ~intersection_point ~normal ~w_o
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
