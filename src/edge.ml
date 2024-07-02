open Core

let square x = x * x |> Float.of_int

let get_pixels sub_image =
  Image.fold ~init:[] sub_image ~f:(fun acc (r, _, _) -> acc @ [ r ])
;;

let conv ~m1 ~m2 =
  List.foldi m1 ~init:0 ~f:(fun y acc i -> acc + (i * List.nth_exn m2 y))
;;

let get_sub_image ~x ~y ~image =
  let width = Image.width image in
  let height = Image.height image in
  let x_start, x_end = max 0 (x - 2), min (width - 1) (x + 2) in
  let y_start, y_end = max 0 (y - 2), min (height - 1) (y + 2) in
  match x_end - x_start, y_end - y_start with
  | 4, 4 -> Some (Image.slice image ~x_start ~x_end ~y_start ~y_end)
  | _ -> None
;;

let get_horizontal_grad sub_image =
  match sub_image with
  | Some sub ->
    let gx = [ -1; 0; 1; -2; 0; 2; -1; 0; 1 ] in
    Some (conv ~m1:gx ~m2:(get_pixels sub))
  | None -> None
;;

let get_vertical_grad sub_image =
  match sub_image with
  | Some sub ->
    let gx = [ -1; -2; -1; 0; 0; 0; 1; 2; 1 ] in
    Some (conv ~m1:gx ~m2:(get_pixels sub))
  | None -> None
;;

(* You need to change the implementation of this function so that it does
   something to the image instead of just leaving it untouched. *)
let transform image ~threshold ~radius =
  let img_max = Image.max_val image in
  let threshold_val = (img_max |> Float.of_int) *. threshold in
  let image = Grayscale.transform image |> Blur.transform ~radius in
  let white, black = (img_max, img_max, img_max), (0, 0, 0) in
  Image.mapi image ~f:(fun ~x ~y _ ->
    let horizontal_gradient =
      get_horizontal_grad (get_sub_image ~image ~x ~y)
    in
    let vertical_gradient = get_vertical_grad (get_sub_image ~image ~x ~y) in
    match horizontal_gradient, vertical_gradient with
    | Some horizontal_gradient, Some vertical_gradient ->
      let final_grad_mag =
        sqrt (square horizontal_gradient +. square vertical_gradient)
      in
      (match Float.O.(final_grad_mag >= threshold_val) with
       | false -> black
       | true -> white)
    | _ -> 0, 0, 0)
;;

let command =
  Command.basic
    ~summary:"Edge Detection"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and threshold =
        flag
          "threshold"
          (required Command.Param.float)
          ~doc:
            "Sets each pixel to black or white based on whether the \
             magnitude exceeds a user-provided threshold"
      and radius =
        flag "radius" (required Command.Param.int) ~doc:"Blur radius"
      in
      fun () ->
        let image =
          Image.load_ppm ~filename |> transform ~threshold ~radius
        in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_edge.ppm")]
;;

(* let%expect_test "edge_test" =
  let img =
    Image.load_ppm ~filename:"../images/beach_portrait.ppm"
    |> transform ~threshold:0.4 ~radius:2
  in
  let ref_img =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_edge.ppm"
  in
  let diff =
    Image.foldi img ~init:[] ~f:(fun ~x ~y diff (r, g, b) ->
      let r1, g1, b1 = Image.get ref_img ~x ~y in
      match r = r1 && b = b1 && g = g1 with
      | false -> (x, y, (r, g, b), (r1, g1, b1)) :: diff
      | true -> diff)
  in
  if List.length diff > 0
  then (
    print_s [%message "x-cord, y-cord, actual, expected\n"];
    print_s [%message (diff : (int * int * Pixel.t * Pixel.t) list)])
;; *)
