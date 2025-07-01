open Core

let calculate_error_and_set x y image error =
  if x + 1 <= Image.width image - 1
  then (
    let x_cord = x + 1 in
    let adjustment =
      Int.to_float error *. 7. /. 16. |> Float.round |> Float.to_int
    in
    Image.set
      ~x:x_cord
      ~y
      image
      Pixel.(Image.get ~x:x_cord ~y image + of_int adjustment))
  else ();
  if x - 1 >= 0 && y + 1 <= Image.height image - 1
  then (
    let x_cord = x - 1 in
    let y_cord = y + 1 in
    let adjustment =
      Int.to_float error *. 3. /. 16. |> Float.round |> Float.to_int
    in
    Image.set
      ~x:x_cord
      ~y:y_cord
      image
      Pixel.(Image.get ~x:x_cord ~y:y_cord image + of_int adjustment))
  else ();
  if y + 1 <= Image.height image - 1
  then (
    let y_cord = y + 1 in
    let adjustment =
      Int.to_float error *. 5. /. 16. |> Float.round |> Float.to_int
    in
    Image.set
      ~x
      ~y:y_cord
      image
      Pixel.(Image.get ~x ~y:y_cord image + of_int adjustment))
  else ();
  if x + 1 <= Image.width image - 1 && y + 1 <= Image.height image - 1
  then (
    let x_cord = x + 1 in
    let y_cord = y + 1 in
    let adjustment =
      Int.to_float error /. 16. |> Float.round |> Float.to_int
    in
    Image.set
      ~x:x_cord
      ~y:y_cord
      image
      Pixel.(Image.get ~x:x_cord ~y:y_cord image + of_int adjustment))
  else ()
;;

(* This should look familiar by now! *)
let transform image =
  let grayscale = Grayscale.transform image in
  let max_val = Image.max_val grayscale in
  Image.mapi grayscale ~f:(fun ~x ~y pixel ->
    let red_pixel = Pixel.red pixel in
    match red_pixel > Image.max_val grayscale / 2 with
    | true ->
      let error = red_pixel - max_val in
      calculate_error_and_set x y grayscale error;
      Pixel.white ~max_val
    | false ->
      let error = red_pixel in
      calculate_error_and_set x y grayscale error;
      Pixel.zero)
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

let%expect_test "ditherr" =
  let filename = "../images/beach_portrait.ppm" in
  let test_file_name = "../images/reference-beach_portrait_dither.ppm" in
  let image = Image.load_ppm ~filename in
  let transformed_img = transform image
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