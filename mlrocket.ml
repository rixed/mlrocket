(* This program is a test program to play with OCaml and the geom library in
 * a "realistic" setting. Don't expect too much. *)

module G = Glop_impl.Glop2D
module View = Glop_view.Make(G)
module Point = Geom_shapes.Point (G.V)
module Bbox = Point.Bbox
module K = Point.K
module Poly = Geom_shapes.Polygon (Point)
module Path = Geom_path.Make (Point)
module Algo = Geom_algo.Algorithms (Poly) (Path)

let pi = 4. *. atan 1.
let k_pi = K.of_float pi

let ang_norm x y =
    let l = sqrt (x*.x +. y*.y) in
    let a =
        if abs_float y > abs_float x then
            let a = acos (x/.l) in
            if y >= 0. then a else 2. *. pi -. a
        else if l > 0. then
            let a = asin (y/.l) in
            if x >= 0. then (
                if y >= 0. then a else 2. *. pi +. a
            ) else pi -. a
        else 0. in
    assert (a >= 0. && a <= 2. *. pi) ;
    a, l

open Format

let mlog fmt =
	kfprintf
		(fun ff ->
			pp_print_newline ff () ;
			pp_print_flush ff ())
		std_formatter fmt

