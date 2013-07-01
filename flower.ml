open Mlrocket

type t =
    { start_pos : G.V.t ;
      stem_rest : Path.t ;
      head_rest_pos : Point.t ;
      mutable stem : Path.t ;
      head : Poly.t ;
      mutable head_pos : Point.t ;
      gc : Pic.gc ;
      bbox : Bbox.t }

let iter_star f ~nb_peaks ~radius ~width =
    let delta_angle = 2. *. pi /. float_of_int nb_peaks in
    let start_angle = Random.float pi in
    let pt a =
        let c = K.of_float (radius *. cos a)
        and s = K.of_float (radius *. sin a) in
        [| c ; s |] in
    let rec loop n a =
        if n > 0 then (
            let am = a -. width /. 2.
            and ap = a +. width /. 2. in
            f (pt am) (pt ap) ;
            loop (n-1) (a +. delta_angle)
        ) in
    loop nb_peaks start_angle
 
let star ~nb_peaks ~radius ~width =
    let poly = ref (Poly.singleton Point.zero) in
    iter_star (fun p0 p1 ->
        poly := Poly.insert_after !poly p0 ;
        poly := Poly.insert_after !poly (Point.add p0 p1) ;
        poly := Poly.insert_after !poly p1 ;
        poly := Poly.insert_after !poly Point.zero)
        ~nb_peaks ~radius ~width ;
    !poly

let soft ~nb_peaks ~radius ~width =
    let path = ref (Path.empty Point.zero) in
    iter_star (fun p0 p1 ->
        path := Path.extend !path Point.zero [ p0 ; p1 ] Path.make_bezier_curve)
        ~nb_peaks ~radius ~width ;
    Algo.poly_of_path !path (K.of_float 0.05)

let make start dir =
    let rand_next p d s =
        let spray = K.to_float (G.V.norm d) *. s in
        Array.mapi (fun i x ->
            let r = K.of_float (Random.float spray *. 2. -. spray) in
            K.add (K.add x d.(i)) r) p in
    let rec rand_line start dir ?(spray=0.5) ?(prev=[]) = function
        | 0 -> prev
        | n ->
            let next = rand_next start dir spray in
            rand_line next (G.V.mul (K.of_float 0.8) dir) ~spray:(spray *. 1.4) ~prev:(next::prev) (n-1) in
    let stem, stem_end, bbox = match rand_line start dir (Random.int 4 + 2) with
        | last :: ctrls ->
            let path = Path.(extend (empty start) last (List.rev ctrls) make_bezier_curve) in
            path, last, Path.bbox path
        | _ -> assert false in
    (* now the flower itself *)
    let head = match Random.int 4 with
        | 0|1|2 ->
            let nb_peaks = 4 + Random.int 5 in
            let radius = Random.float 1. +. 2.5 in
            let width = Random.float 0.6 +. 0.3 in
            soft ~nb_peaks ~radius ~width
        | _ ->
            let radius = Random.float 1. +. 1. in
            let nb_peaks = int_of_float (radius *. 8.) + Random.int 5 in
            let width = 0.1 in
            star ~nb_peaks ~radius ~width in
    let bbox = Bbox.union bbox (Algo.bbox_single_poly head) in
    let gc = Pic.uni_gc (Pic.rand_col ()) in
    { start_pos = start ;
      stem_rest = stem ;
      head_rest_pos = stem_end ;
      stem ; (* will be recomputed by run *)
      head ;
      head_pos = stem_end ; (* will be recomputed by run *)
      bbox ; gc }

let draw prec t =
    let open Pic in
    let head = Poly.translate t.head t.head_pos in
    draw ~prec [ Path t.stem, t.gc ; Poly head, t.gc ]

let run now _dt radius weather t =
    let wind_speed = Weather.wind_speed weather t.head_rest_pos radius now in
    (* rebuilt stem from stem_rest *)
    t.stem <- Path.map_pts (fun stop ctrls ->
        let stop' = G.V.add wind_speed stop in
        let ctrls' = List.map (fun ctrl -> G.V.add wind_speed ctrl) ctrls in
        t.head_pos <- stop' ;
        stop', ctrls') t.stem_rest
