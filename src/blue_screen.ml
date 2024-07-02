open Core

(* You need to change the implementation of this function so that it replaces
   the "blue" pixels of the foreground image with pixels from the
   corresponding position in the background image instead of just ignoring
   the background image and returning the foreground image. *)
let transform ~foreground ~background =
  Image.mapi foreground ~f:(fun ~x ~y (r, g, b) ->
    match b > r + g with
    | true -> Image.get background ~x ~y
    | false -> r, g, b)
;;

let _transform_improved ~foreground ~background =
  Image.mapi foreground ~f:(fun ~x ~y (r, g, b) ->
    match b > r + g with
    | true -> Image.get background ~x ~y
    | false -> r, g, b)
;;

let command =
  Command.basic
    ~summary:
      "Replace the 'blue' pixels of an image with those from another image"
    [%map_open.Command
      let foreground_file =
        flag
          "foreground"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the foreground PPM image file"
      and background_file =
        flag
          "background"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the background PPM image file"
      in
      fun () ->
        let foreground = Image.load_ppm ~filename:foreground_file in
        let background = Image.load_ppm ~filename:background_file in
        let image' = transform ~foreground ~background in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn foreground_file ~suffix:".ppm"
             ^ "_vfx.ppm")]
;;

let%expect_test "bluescreen_test" =
  let foreground = Image.load_ppm ~filename:"../images/oz_bluescreen.ppm" in
  let background = Image.load_ppm ~filename:"../images/meadow.ppm" in
  let img = transform ~foreground ~background in
  let ref_img =
    Image.load_ppm ~filename:"../images/reference-oz_bluescreen_vfx.ppm"
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
(* 
let%expect_test "bluescreen_improved_test" =
  let foreground = Image.load_ppm ~filename:"../images/oz_bluescreen.ppm" in
  let background = Image.load_ppm ~filename:"../images/meadow.ppm" in
  let img = transform_improved ~foreground ~background in
  let ref_img =
    Image.load_ppm ~filename:"../images/reference-oz_bluescreen_vfx_improved.ppm"
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
