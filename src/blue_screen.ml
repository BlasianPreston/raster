open Core

(* You need to change the implementation of this function so that it
   replaces the "blue" pixels of the foreground image with pixels from
   the corresponding position in the background image instead of
   just ignoring the background image and returning the foreground image.
*)
let transform ~foreground ~background =
  Image.mapi foreground ~f:(fun ~x ~y pixel ->
    if Pixel.blue pixel > Pixel.red pixel + Pixel.green pixel
    then Image.get background ~x ~y
    else pixel)
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

let%expect_test "blue_screen" =
  let filename = "../images/oz_bluescreen.ppm" in
  let test_file_name = "../images/reference-oz_bluescreen_vfx.ppm" in
  let background = Image.load_ppm ~filename:"../images/meadow.ppm" in
  let image = Image.load_ppm ~filename in
  let transformed_img = transform ~background ~foreground:image
in
let expected_image = Image.load_ppm ~filename:test_file_name in
let result =
  if
    Image.width transformed_img = Image.width expected_image
    && Image.height transformed_img = Image.height expected_image
  then
    Image.foldi transformed_img ~init:0 ~f:(fun ~x ~y acc pixel ->
      if Image.test_helper x y expected_image pixel then acc else acc + 1)
  else -1
in
print_s [%message (result : int)];
[%expect {| (result 0) |}]
