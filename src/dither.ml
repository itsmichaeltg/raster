open Core

let distribute_error ~x ~y ~error ~image ~factor =
  let r, _, _ = Image.get ~x ~y image in
  let r =
    Float.O.(Float.of_int r + (factor / 16.0 * error))
    |> Float.round_nearest
    |> Int.of_float
  in
  Image.set ~x ~y image (r, r, r)
;;

let update ~x ~y ~error ~image =
  let height, width = Image.height image, Image.width image in
  if x + 1 < width
  then distribute_error ~x:(x + 1) ~y ~error ~image ~factor:7.0;
  if y + 1 < height
  then distribute_error ~x ~y:(y + 1) ~error ~image ~factor:5.0;
  if x - 1 >= 0 && y + 1 < height
  then distribute_error ~x:(x - 1) ~y:(y + 1) ~error ~image ~factor:3.0;
  if y + 1 < height && x + 1 < width
  then distribute_error ~x:(x + 1) ~y:(y + 1) ~error ~image ~factor:1.0
;;

(* This should look familiar by now! *)
let transform image =
  let image = Grayscale.transform image in
  let _k =
    Image.foldi image ~init:0 ~f:(fun ~x ~y _ (r, _, _) ->
      (match Float.O.(Float.of_int r > 65535.0 / 2.0) with
       | true ->
         Image.set image ~x ~y (65535, 65535, 65535);
         update ~x ~y ~error:Float.O.(Float.of_int r - 65535.0) ~image
       | false ->
         Image.set image ~x ~y (0, 0, 0);
         update ~x ~y ~error:(Float.of_int r) ~image);
      0)
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;

(* let%expect_test "dither_test" =
  let img =
    Image.load_ppm ~filename:"../images/beach_portrait.ppm" |> transform
  in
  let ref_img =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_dither.ppm"
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
    (* print_s [%message (diff : (int * int * Pixel.t * Pixel.t) list)]) *)
    print_s [%message (List.length diff : int)]) *)
;;
