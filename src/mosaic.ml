open Core

let square x = x * x |> Float.of_int

let mean_squared_error ~image_1 ~image_2 =
  let height, width = Image.height image_1, Image.width image_1 in
  let factor = 1.0 /. (height * width |> Float.of_int) in
  Image.foldi image_1 ~init:(0.0, 0.0, 0.0) ~f:(fun ~x ~y _ (r, g, b) ->
    let r_1, g_1, b_1 = Image.get image_2 ~x ~y in
    ( factor *. square (r - r_1)
    , factor *. square (g - g_1)
    , factor *. square (b - b_1) ))
;;

let _get_sub_image ~x ~y ~image ~width ~height =
  let img_width = Image.width image in
  let img_height = Image.height image in
  let x_start, x_end = max 0 (x - width), min (img_width - 1) (x + width) in
  let y_start, y_end =
    max 0 (y - height), min (img_height - 1) (y + height)
  in
  Image.slice image ~x_start ~x_end ~y_start ~y_end
;;

let mag v1 =
  let x1, y1, z1 = v1 in
  sqrt ((x1 *. x1) +. (y1 *. y1) +. (z1 *. z1))
;;

let _find_similar ~image ~sub_images =
  let min_img, _min_mse =
    List.fold
      ~init:(image, Float.max_value)
      sub_images
      ~f:(fun (min_img, min_mse) img ->
        let tmp = mag (mean_squared_error ~image_1:image ~image_2:img) in
        match Float.O.(tmp < min_mse) with
        | true -> img, tmp
        | false -> min_img, min_mse)
  in
  min_img
;;

let transform image ~moves:_ ~width:_ ~height:_ = image

let command =
  Command.basic
    ~summary:"Convert an image to mosaic"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and moves =
        flag
          "moves"
          (required Command.Param.int)
          ~doc:"the number of moves to perform"
      and width =
        flag
          "width"
          (required Command.Param.int)
          ~doc:"the width of the regions to move"
      and height =
        flag
          "height"
          (required Command.Param.int)
          ~doc:"the height of the regions to move"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~moves ~width ~height in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_mosaic.ppm")]
;;
