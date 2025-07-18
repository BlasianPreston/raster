open! Core

type t = int * int * int [@@deriving sexp]

let red (r, _, _) = r
let green (_, g, _) = g
let blue (_, _, b) = b
let zero = 0, 0, 0
let white ~max_val = max_val, max_val, max_val
let red_pix ~max_val = max_val, 0, 0
let green_pix ~max_val = 0, max_val, 0
let blue_pix ~max_val = 0, 0, max_val
let ( + ) (r1, g1, b1) (r2, g2, b2) = r1 + r2, g1 + g2, b1 + b2
let ( - ) (r1, g1, b1) (r2, g2, b2) = r1 - r2, g1 - g2, b1 - b2
let ( * ) (r1, g1, b1) x = r1 * x, g1 * x, b1 * x
let ( / ) (r1, g1, b1) x = r1 / x, g1 / x, b1 / x
let of_int i = i, i, i
let to_string (r, g, b) = [%string "%{r#Int} %{g#Int} %{b#Int}"]
let to_color (r, g, b) = Graphics.rgb r g b
let of_color c = c lsr 16, (c lsr 8) land 0xff, c land 0xff
let equal (r1, g1, b1) (r2, g2, b2) = r1 = r2 && g1 = g2 && b1 = b2
let create (r, g, b) = (r, g, b)
