open Core

let transform_helper x y radius image =
  let x_end = if x + radius > Image.width image - 1 then Image.width image - 1 else x + radius in
  let x_start = if x - radius < 0 then 0 else x - radius in
  let y_end = if y + radius > Image.height image - 1 then Image.height image - 1 else y + radius in
  let y_start = if y - radius < 0 then 0 else y - radius in
  Image.mean_pixel (Image.slice ~x_start ~x_end ~y_start ~y_end image) 

(* You need to modify this function to blur the input image
   based on the provided radius instead of ignoring it. *)
let transform image ~radius = Image.mapi image ~f:(fun ~x ~y _ -> transform_helper x y radius image)

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

let%expect_test "blur" =
  let filename = "../images/beach_portrait.ppm" in
  let test_file_name = "../images/reference-beach_portrait_blur.ppm" in
  let image = Image.load_ppm ~filename in
  let transformed_img = transform image ~radius:3
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