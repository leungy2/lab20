type image = float list list ;;
(* images are lists of lists of floats between 0. (white) and 1. (black) *)
type size = int * int ;;
open Graphics ;;

(* threshold image -- image where pixels above the threshold
value are black *)

let threshold (img : float list list) (threshold : float) : float list list =
  let pixel_threshold (f : float) : float =
  if f <= threshold then 0. else 1. in
  List.map (fun row -> List.map pixel_threshold row) img ;;

(* dither max image -- dithered image *)
let dither (img : float list list) : float list list =
  let dither_threshold (f : float) : float =
    if f > Random.float 1. then 1. else 0. in
  List.map (fun row -> List.map dither_threshold row) img ;;

(* show the image *)
let depict (img : float list list) : unit =
  open_graph "";
  clear_graph ();
  let width, height = List.length (List.hd img), List.length img in
    resize_window width height;
  let depict_pix (pixel : float) (row_count : int) (column : int) : unit =
    let col = int_of_float (255. *. (1. -. pixel)) in set_color (rgb col col col);
  plot column (height - row_count) in
  List.iteri (fun row_count row -> List.iteri (fun column pix -> depict_pix pix row_count column) row) img;
  Unix.sleep 2;
  close_graph () ;;

let mona = Monalisa.image ;;
  depict mona ;;
let mona_threshold = threshold mona 0.75 ;;
  depict mona_threshold ;;
let mona_dither = dither mona ;;
  depict mona_dither ;;

