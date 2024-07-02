open Core

let get_new_val ~img_max ~pixel ~threshold =
  let r, g, b = pixel in
  let sum = img_max - r + (img_max - g) + (img_max - b) in
  let avg = Float.of_int sum /. 3. |> Float.round_up |> Float.to_int in
  Tuple3.map pixel ~f:(fun i ->
    match i >= threshold with true -> avg | false -> i)
;;

(* You need to change the implementation of this function so that it does
   something to the image instead of just leaving it untouched. *)
let _old_transform image ~threshold =
  let img_max = Image.max_val image in
  Image.map image ~f:(fun (r, g, b) ->
    match r >= threshold || g >= threshold || b >= threshold with
    | true ->
      let r, g, b = get_new_val ~img_max ~pixel:(r, g, b) ~threshold in
      r, g, b
    | false -> r, g, b)
;;

let transform image ~threshold =
  let img_max = Image.max_val image in
  let threshold = (threshold *. (img_max |> Float.of_int)) |> Float.to_int in
  Image.map image ~f:(fun (r, g, b) ->
    let r = if r >= threshold then img_max - r else r in
    let g = if g >= threshold then img_max - g else g in
    let b = if b >= threshold then img_max - b else b in
    r,g,b)
;;

let command =
  Command.basic
    ~summary:"Solarize an image"
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
          ~doc:"if a color is above the threshold, the color is inverted"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~threshold in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm"
             ^ "_solarized.ppm")]
;;

let%expect_test "solarize_test" =
  let img =
    Image.load_ppm ~filename:"../images/meadow.ppm"
    |> transform ~threshold:0.4
  in
  let ref_img =
    Image.load_ppm ~filename:"../images/reference-meadow_solarize.ppm"
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
