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

let get_sub_image ~x ~y ~image ~width ~height =
  let width, height = width / 2, height / 2 in
  let img_width = Image.width image in
  let img_height = Image.height image in
  let x_start, x_end = max 0 (x - width), min (img_width - 1) (x + (width + 1)) in
  let y_start, y_end =
    max 0 (y - height), min (img_height - 1) (y + (height + 1))
  in
  match x_end - x_start, y_end - y_start with
  | x_diff, y_diff when x_diff = 2 * width && y_diff = 2 * height ->
    Some ((x, y), Image.slice image ~x_start ~x_end ~y_start ~y_end)
  | _ -> None
;;

let mag v1 =
  let x1, y1, z1 = v1 in
  sqrt ((x1 *. x1) +. (y1 *. y1) +. (z1 *. z1))
;;

let find_similar ~orig ~image ~sub_images =
  let min_img, _ =
    List.fold
      ~init:(((0, 0), image), Float.max_value)
      sub_images
      ~f:(fun ((min_idx, min_img), min_mse) (cord, img) ->
        let tmp = mag (mean_squared_error ~image_1:image ~image_2:img) in
        match Float.O.(tmp < min_mse), not (Tuple2.equal ~eq1:Int.equal ~eq2:Int.equal cord orig) with
        | true, true -> (cord, img), tmp
        | _ -> (min_idx, min_img), min_mse)
  in
  min_img
;;

let get_sub_regions ~image ~width ~height =
  Image.foldi image ~init:[] ~f:(fun ~x ~y acc _ ->
    match get_sub_image ~x ~y ~image ~width ~height with
    | Some elem -> elem :: acc
    | None -> acc)
;;

let swap coord_1 coord_2 ~image =
  List.iter2_exn coord_2 coord_1 ~f:(fun (x_2, y_2) (x_1, y_1) ->
    let tmp_pixel = Image.get image ~x:x_2 ~y:y_2 in
    Image.set image ~x:x_2 ~y:y_2 (Image.get image ~x:x_1 ~y:y_1);
    Image.set image ~x:x_1 ~y:y_2 tmp_pixel)
;;

let rec get_nums start limit =
  match start < limit with
  | true -> get_nums (start + 1) limit @ [ start ]
  | false -> []
;;

let get_coords x_start x_end y_start y_end =
  let x_range = get_nums x_start x_end in
  let y_range = get_nums y_start y_end in
  List.fold x_range ~init:[] ~f:(fun acc x ->
    acc @ List.fold y_range ~init:[] ~f:(fun acc_2 y -> acc_2 @ [ x, y ]))
;;

let run image ~width ~height =
  let img_width, img_height = Image.width image, Image.height image in
  let x_start, y_start = Random.int width, Random.int height in
  let x_end =
    if x_start + width < img_width then x_start + width else x_start - width
  in
  let y_end =
    if y_start + height < img_height
    then y_start + height
    else y_start - height
  in
  let region_1 = Image.slice image ~x_start ~x_end ~y_start ~y_end in
  let targets = get_sub_regions ~image ~width ~height in
  let (x, y), _region_2 = find_similar ~orig:(x_start, y_start) ~image:region_1 ~sub_images:targets in
  let x_start2, x_end2 = x - (width / 2), x + (width / 2) in
  let y_start2, y_end2 = y - (height / 2), y + (height / 2) in
  let coord_1 = get_coords x_start x_end y_start y_end in
  let coord_2 = get_coords x_start2 x_end2 y_start2 y_end2 in
  swap coord_1 coord_2 ~image
;;

let rec transform image ~moves ~width ~height =
  match moves > 0 with
  | true ->
    run image ~width ~height;
    transform image ~moves:(moves - 1) ~width ~height
  | false -> image
;;

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
