open Core

let calculate_black_and_set x y image =
  let pixel = Image.get image ~x ~y in
  if
    Pixel.red pixel + Pixel.green pixel + Pixel.blue pixel
    > Image.max_val image / 2
  then Image.set ~x ~y image (Pixel.of_color Graphics.white)
  else Image.set ~x ~y image Pixel.zero
;;

let calculate_black x y image =
  let pixel = Image.get image ~x ~y in
  if
    Pixel.red pixel + Pixel.green pixel + Pixel.blue pixel
    > Image.max_val image / 2
  then Pixel.of_color Graphics.white
  else Pixel.zero
;;

let calculate_error_and_set x y image old_image =
  let (error : Pixel.t) =
    Pixel.(Image.get ~x ~y old_image - Image.get ~x ~y image)
  in
  if x + 1 < Image.width image - 1
  then
    Image.set
      ~x:(x + 1)
      ~y
      image
      Pixel.(Image.get ~x ~y image + (error * 7 / 16))
  else ();
  if x - 1 >= 0 && y - 1 >= 0
  then
    Image.set
      ~x:(x - 1)
      ~y:(y - 1)
      image
      Pixel.(Image.get ~x ~y image + (error * 3 / 16))
  else ();
  if y - 1 >= 0
  then
    Image.set
      ~x
      ~y:(y - 1)
      image
      Pixel.(Image.get ~x ~y image + (error * 5 / 16))
  else ();
  if x + 1 >= Image.width image - 1 && y - 1 >= 0
  then
    Image.set
      ~x:(x - 1)
      ~y:(y - 1)
      image
      Pixel.(Image.get ~x ~y image + (error / 16))
  else ()
;;

(* This should look familiar by now! *)
let transform image =
  let grayscale = Grayscale.transform image in
  let copy = Image.copy grayscale in
  Image.mapi grayscale ~f:(fun ~x ~y _ ->
    calculate_black_and_set x y grayscale;
    calculate_error_and_set x y grayscale copy;
    calculate_black x y grayscale)
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
