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

let sample_ray_triangle ~scene:_ ~x:_ ~y:_ ~ray ~triangle:_ ~distance =
  Some (Radiance3.of_color
          (Color3.of_vector
             Vector3.(scale (Ray.direction ray + create ~x:1.0 ~y:1.0 ~z:1.0)
                        (1.0 /. 5.0))),
        distance)
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
            match sample_ray_triangle ~scene ~x ~y ~ray:eye_ray ~triangle ~distance with
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
