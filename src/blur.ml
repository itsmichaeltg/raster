open Core

let get_sub_image ~x ~y ~image ~radius = 
  let width = Image.width image in
  let height = Image.height image in
  let x_start, x_end = max 0 (x - radius), min (width - 1) (x + radius) in
  let y_start, y_end = max 0 (y - radius), min (height - 1) (y + radius) in
  Image.slice image ~x_start ~x_end ~y_start ~y_end 
;;

(* You need to modify this function to blur the input image
   based on the provided radius instead of ignoring it. *)
let transform image ~radius = 
  Image.mapi image ~f:(fun ~x ~y _ -> get_sub_image ~x ~y ~image ~radius |> Image.mean_pixel)
;;

let command =
  Command.basic
    ~summary:"Blur an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and radius =
        flag
          "radius"
          (required Command.Param.int)
          ~doc:"N the radius to use when blurring (higher = more blurred)"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~radius in
        Image.save_ppm
          image'
          ~filename:(String.chop_suffix_exn filename ~suffix:".ppm" ^ "_blur.ppm")]
;;

let%expect_test "blur_test" =
  let img =
    Image.load_ppm ~filename:"../images/beach_portrait.ppm" |> transform ~radius:3 
  in
  let ref_img =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_blur.ppm"
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
;;