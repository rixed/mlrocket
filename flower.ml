open Mlrocket

type t =
    { pos : G.V.t ;
      paths : Path.t list ;
      bbox : Bbox.t ;
      gc : Pic.gc }

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
    let stem, stem_end = match rand_line start dir (Random.int 4 + 2) with
        | last :: ctrls ->
            Path.(extend (empty start) last (List.rev ctrls) make_bezier_curve), last
        | _ -> assert false in
    (* now the flower itself *)
    let head = match Random.int 4 with
        | 1|2 ->
            let pi = K.to_float pi in
            let nb_petals = 3 + Random.int 4 in
            let delta_angle = 2. *. pi /. float_of_int nb_petals in
            let start_angle = Random.float pi in
            let radius = K.of_float (Random.float 1. +. 2.) in
            let make_ctrl a =
                let c = K.of_float (cos a)
                and s = K.of_float (sin a) in
                [| K.add stem_end.(0) (K.mul radius c) ;
                   K.add stem_end.(1) (K.mul radius s) |] in
            let rec add_petal path ang = function
                | 0 -> path
                | n ->
                    let path = Path.(extend path stem_end
                                [ make_ctrl (ang -. 0.4 *. delta_angle) ;
                                  make_ctrl (ang +. 0.4 *. delta_angle) ]
                                make_bezier_curve) in
                    add_petal path (ang +. delta_angle) (n-1) in
            [ add_petal (Path.empty stem_end) start_angle nb_petals ]
        | _ ->
            let h = ref [] in
            let radius = Random.float 1. +. 1. in
            for _i = 0 to int_of_float (radius *. 8.) do
                let angle = Random.float (2.*.(K.to_float pi)) in
                let radius = K.of_float (radius *. (Random.float 0.2 +. 0.9)) in
                let c = K.mul radius (K.of_float (cos angle))
                and s = K.mul radius (K.of_float (sin angle)) in
                let p0 = [| K.add stem_end.(0) c ; K.add stem_end.(1) s |]
                and p1 = [| K.sub stem_end.(0) c ; K.sub stem_end.(1) s |] in
                h := Path.(extend (empty p0) p1 [] make_straight_line) :: !h
            done ;
            !h in
    let paths = stem :: head in
    let bbox = List.fold_left (fun bbox p -> Bbox.union (Path.bbox p) bbox) Bbox.empty paths in
    { pos = start ; paths ; bbox ; gc = Pic.uni_gc (Pic.rand_col ()) }

