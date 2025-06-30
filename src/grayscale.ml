open Core

(* You need to change the implementation of this function so that it does something
   to the image instead of just leaving it untouched. *)
let transform image =
  Image.map image ~f:(fun (r, g, b) ->
    let gray = (r + g + b) / 3 in
    gray, gray, gray)
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

let test_helper x y image pixel =
  let expected_result = Image.get image ~x ~y in
  Pixel.equal expected_result pixel
;;

let%expect_test ("grayscale" ) =
  let filename = "../images/beach_portrait.ppm" in
  let test_file_name = "../images/reference-beach_portrait_gray.ppm" in
  let image = Image.load_ppm ~filename |> transform in
  let expected_image = Image.load_ppm ~filename:test_file_name in
  let result =
    if
      Image.width image = Image.width expected_image
      && Image.height image = Image.height expected_image
    then
      Image.foldi image ~init:0 ~f:(fun ~x ~y acc pixel ->
        if test_helper x y expected_image pixel then acc else acc + 1)
    else -1
  in
  print_s [%message (result : int)];
  [%expect {| (result 0) |}]
;;
