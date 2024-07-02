open Core

let distribute_error ~x ~y ~error ~image ~factor ~color =
  let r_bool, g_bool, b_bool = color in
  let r, g, b = Image.get ~x ~y image in
  let r =
    if r_bool
    then
      Float.O.(Float.of_int r + (factor / 16.0 * error))
      |> Float.round_nearest
      |> Int.of_float
    else r
  in
  let g =
    if g_bool
    then
      Float.O.(Float.of_int g + (factor / 16.0 * error))
      |> Float.round_nearest
      |> Int.of_float
    else g
  in
  let b =
    if b_bool
    then
      Float.O.(Float.of_int b + (factor / 16.0 * error))
      |> Float.round_nearest
      |> Int.of_float
    else b
  in
  Image.set ~x ~y image (r, g, b)
;;

let update ~x ~y ~error ~image ~color =
  let height, width = Image.height image, Image.width image in
  if x + 1 < width
  then distribute_error ~x:(x + 1) ~y ~error ~image ~factor:7.0 ~color;
  if y + 1 < height
  then distribute_error ~x ~y:(y + 1) ~error ~image ~factor:5.0 ~color;
  if x - 1 >= 0 && y + 1 < height
  then
    distribute_error ~x:(x - 1) ~y:(y + 1) ~error ~image ~factor:3.0 ~color;
  if y + 1 < height && x + 1 < width
  then
    distribute_error ~x:(x + 1) ~y:(y + 1) ~error ~image ~factor:1.0 ~color
;;

(* This should look familiar by now! *)
let transform image =
  let img_max = Image.max_val image in
  let threshold = (img_max |> Float.of_int) /. 2. in
  let _k =
    Image.foldi image ~init:() ~f:(fun ~x ~y _ (r, g, b) ->
      let r =
        if Float.O.(r |> Float.of_int >= threshold)
        then (
          update
            ~x
            ~y
            ~error:(r - img_max |> Float.of_int)
            ~image
            ~color:(true, false, false);
          img_max)
        else (
          update
            ~x
            ~y
            ~error:(r - 0 |> Float.of_int)
            ~image
            ~color:(true, false, false);
          0)
      in
      let g =
        if Float.O.(g |> Float.of_int >= threshold)
        then (
          update
            ~x
            ~y
            ~error:(g - img_max |> Float.of_int)
            ~image
            ~color:(false, true, false);
          img_max)
        else (
          update
            ~x
            ~y
            ~error:(g - 0 |> Float.of_int)
            ~image
            ~color:(false, true, false);
          0)
      in
      let b =
        if Float.O.(b |> Float.of_int >= threshold)
        then (
          update
            ~x
            ~y
            ~error:(b - img_max |> Float.of_int)
            ~image
            ~color:(false, false, true);
          img_max)
        else (
          update
            ~x
            ~y
            ~error:(b - 0 |> Float.of_int)
            ~image
            ~color:(false, false, true);
          0)
      in
      Image.set image ~x ~y (r, g, b))
  in
  image
;;

let command =
  Command.basic
    ~summary:"Dither an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm"
             ^ "_dither_color.ppm")]
;;

let%expect_test "dither_color_test" =
  let img =
    Image.load_ppm ~filename:"../images/beach_portrait.ppm" |> transform
  in
  let img_max = Image.max_val img in
  let diff =
    Image.foldi img ~init:[] ~f:(fun ~x ~y diff (r, g, b) ->
      match
        (not (r = 0))
        || (not (r = img_max))
        || (not (g = 0))
        || (not (g = img_max))
        || (not (b = 0))
        || not (b = img_max)
      with
      | false -> (x, y, (r, g, b)) :: diff
      | true -> diff)
  in
  if List.length diff > 0
  then (
    print_s [%message "x-cord, y-cord, actual,\n   expected\n"];
    print_s [%message (diff : (int * int * Pixel.t) list)];
    print_s [%message (List.length diff : int)])
;;

(* Compare with ref let%expect_test "dither_color_test" = let img =
   Image.load_ppm ~filename:"../images/beach_portrait.ppm" |> transform in
   let ref_img = Image.load_ppm
   ~filename:"../images/reference-beach_portrait_dither_color.ppm" in let
   diff = Image.foldi img ~init:[] ~f:(fun ~x ~y diff (r, g, b) -> let r1,
   g1, b1 = Image.get ref_img ~x ~y in match r = r1 && b = b1 && g = g1 with
   | false -> (x, y, (r, g, b), (r1, g1, b1)) :: diff | true -> diff) in if
   List.length diff > 0 then ( print_s [%message "x-cord, y-cord, actual,
   expected\n"]; (* print_s [%message (diff : (int * int * Pixel.t * Pixel.t)
   list)]) *) print_s [%message (List.length diff : int)]) ;; *)
