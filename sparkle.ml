open Mlrocket

type t =
    { mutable pos : Point.t ;
      speed : G.V.t ;
      mutable life : int ;
      gc : Pic.gc }

let make pos orient spray speed =
    let orient = Array.map (fun x ->
        let s = (Random.float spray) -. spray/.2. in
        K.add x (K.of_float s)) orient in
    let renorm v =
        let n = G.V.norm v in
        if K.compare n K.zero > 0 then Array.map (fun x -> K.div x n) v
        else v in
    let orient = renorm orient in
    let speed = K.mul (K.of_float (Random.float 0.4 +. 0.8)) speed in
    let speed = G.V.mul speed orient in
    let rand_col () =
        [| K.of_float (0.8 +. Random.float 0.2) ;
           K.of_float (0.5 +. Random.float 0.5) ;
           K.of_float (0.  +. Random.float 1.) |] in
    let rand_gc () =
        let c = G.Uniq (rand_col ()) in
        { Pic.fill_color = Some c ; Pic.outline_color = Some c } in
    { speed ; pos ; life = 3 + Random.int 10 ; gc = rand_gc () }

let run _gravity dt t =
    let s = G.V.mul dt t.speed in
    t.pos <- G.V.add t.pos s ;
    t.life <- t.life-1

