open Core

(* You need to change the implementation of this function so that it does
   something to the image instead of just leaving it untouched. *)
let transform image =
  Image.map image ~f:(fun (r, g, b) ->
    let avg = (r + g + b) / 3 in
    avg, avg, avg)
;;

let command =
  Command.basic
    ~summary:"Convert an image to grayscale"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_gray.ppm")]
;;

let%expect_test "grayscale_test" =
  let img =
    Image.load_ppm ~filename:"../images/beach_portrait.ppm" |> transform
  in
  let ref_img =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_gray.ppm"
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
