open Core

let apply_adjustment ~x ~y ~image ~adjustment =
  if x > Image.width image || x < 0 || y > Image.height image || y < 0
  then ()
  else Image.set ~x ~y image Pixel.(Image.get ~x ~y image + adjustment)
;;

(* CR leli: Replace below with apply_adjustment *)

let calculate_error_and_set x y image (red_error, green_error, blue_error) =
  if x + 1 <= Image.width image - 1
  then (
    let x_cord = x + 1 in
    let red_adjustment =
      Int.to_float red_error *. 7. /. 16. |> Float.round |> Float.to_int
    in
    let green_adjustment =
      Int.to_float green_error *. 7. /. 16. |> Float.round |> Float.to_int
    in
    let blue_adjustment =
      Int.to_float blue_error *. 7. /. 16. |> Float.round |> Float.to_int
    in
    let adjustment =
      Pixel.create (red_adjustment, green_adjustment, blue_adjustment)
    in
    apply_adjustment ~x:x_cord ~y ~image ~adjustment)
  else ();
  if x - 1 >= 0 && y + 1 <= Image.height image - 1
  then (
    let x_cord = x - 1 in
    let y_cord = y + 1 in
    let red_adjustment =
      Int.to_float red_error *. 3. /. 16. |> Float.round |> Float.to_int
    in
    let green_adjustment =
      Int.to_float green_error *. 3. /. 16. |> Float.round |> Float.to_int
    in
    let blue_adjustment =
      Int.to_float blue_error *. 3. /. 16. |> Float.round |> Float.to_int
    in
    let adjustment =
      Pixel.create (red_adjustment, green_adjustment, blue_adjustment)
    in
    apply_adjustment ~x:x_cord ~y:y_cord ~image ~adjustment)
  else ();
  if y + 1 <= Image.height image - 1
  then (
    let y_cord = y + 1 in
    let red_adjustment =
      Int.to_float red_error *. 5. /. 16. |> Float.round |> Float.to_int
    in
    let green_adjustment =
      Int.to_float green_error *. 5. /. 16. |> Float.round |> Float.to_int
    in
    let blue_adjustment =
      Int.to_float blue_error *. 5. /. 16. |> Float.round |> Float.to_int
    in
    let adjustment =
      Pixel.create (red_adjustment, green_adjustment, blue_adjustment)
    in
    apply_adjustment ~x ~y:y_cord ~image ~adjustment)
  else ();
  if x + 1 <= Image.width image - 1 && y + 1 <= Image.height image - 1
  then (
    let x_cord = x + 1 in
    let y_cord = y + 1 in
    let red_adjustment =
      Int.to_float red_error /. 16. |> Float.round |> Float.to_int
    in
    let green_adjustment =
      Int.to_float green_error /. 16. |> Float.round |> Float.to_int
    in
    let blue_adjustment =
      Int.to_float blue_error /. 16. |> Float.round |> Float.to_int
    in
    let adjustment =
      Pixel.create (red_adjustment, green_adjustment, blue_adjustment)
    in
    apply_adjustment ~x:x_cord ~y:y_cord ~image ~adjustment)
  else ()
;;

let is_red_max pixel =
  if
    Pixel.red pixel > Pixel.blue pixel && Pixel.red pixel > Pixel.green pixel
  then true
  else false
;;

let is_blue_max pixel =
  if
    Pixel.blue pixel > Pixel.red pixel
    && Pixel.blue pixel > Pixel.green pixel
  then true
  else false
;;

let is_green_max pixel =
  if
    Pixel.green pixel > Pixel.blue pixel
    && Pixel.green pixel > Pixel.red pixel
  then true
  else false
;;

let rec generate_list_from_0_to_n n x =
  if x <= n then x :: generate_list_from_0_to_n n (x + 1) else []
;;

let closest_in_list value list =
  let (_, closest) = List.fold list ~init:(Int.max_value, Int.max_value) ~f:(fun (lowest_diff, matching_val) x ->
    let diff = Int.abs (value - x) in
    print_endline (Int.to_string diff);
    if (Int.min diff lowest_diff) = diff then (diff, x) else (lowest_diff, matching_val)) in closest
;;

let round_to_nearest_threshold color_val max_val channels =
  let threshold_multiple = max_val / channels in
  let list_of_thresholds =
    List.map (generate_list_from_0_to_n channels 0) ~f:(fun x ->
      x * threshold_multiple)
  in
  let num = closest_in_list color_val list_of_thresholds in
  print_endline (List.to_string list_of_thresholds ~f:Int.to_string);
  num
;;

let transform image channels =
  let channels = channels - 1 in
  let max_val = Image.max_val image in
  Image.mapi image ~f:(fun ~x ~y pixel ->
    (* CR leli: Using red is a bit confusing to represent all 3 colors. Add a comment for why we are only looking at red *)
    let red_pixel = Pixel.red pixel in
    let blue_pixel = Pixel.blue pixel in
    let green_pixel = Pixel.green pixel in
    (* Update this so that it compares all values to the max value *)
    let error =
      Pixel.( - )
        pixel
        (Pixel.create
           ( red_pixel - round_to_nearest_threshold red_pixel max_val channels
           , green_pixel
             - round_to_nearest_threshold green_pixel max_val channels
           , blue_pixel
             - round_to_nearest_threshold blue_pixel max_val channels ))
    in
    calculate_error_and_set x y image error;
    Pixel.create
      ( round_to_nearest_threshold red_pixel max_val channels
      , round_to_nearest_threshold green_pixel max_val channels
      , round_to_nearest_threshold blue_pixel max_val channels ))
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
        let image = Image.load_ppm ~filename in
        let transformed_image = transform image 2 in
        Image.save_ppm
          transformed_image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm"
             ^ "_color_dither.ppm")]
;;

let%expect_test "ditherr" =
  let filename = "../images/beach_portrait.ppm" in
  let test_file_name = "../images/reference-beach_portrait_dither.ppm" in
  let image = Image.load_ppm ~filename in
  let transformed_img = transform image 2 in
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
;;
