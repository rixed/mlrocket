open Mlrocket

(* We need a smooth, evolving, periodic function for wind parameters.
 * This function is then modulated by a function of radius and a function
 * of time (which ensure 0 wind a some time t0 where we can change any
 * function at will to change weather *)

type wave =
    { length : float ; (* nb periods * pi *)
      strength : float }

type t =
    { x : wave list ;
      rad : wave list ;
      time : wave list ;
      start : float ;
      stop : float (* end of these weather conditions *) }

let make now =
    let nb_waves () = 1 + Random.int 3 in
    let random_wave nb_periods_min nb_periods_max max_strength =
        { length = pi *. (float_of_int (nb_periods_min + Random.int (nb_periods_max-nb_periods_min))) ;
          strength = Random.float max_strength } in
    let random_waves nb_periods_min nb_periods_max max_strength =
        let rec aux prev p_min p_max = function
            | 0 -> prev
            | n ->
                let w = random_wave p_min p_max max_strength in
                aux (w::prev) p_min p_max (n-1) in
        aux [] nb_periods_min nb_periods_max (nb_waves ()) in
    { x = random_waves 6 12 4. ;
      rad = random_waves 1 5 0.5 ;
      time = random_waves 1 10 1. ;
      start = now ;
      stop = now +. 10. +. Random.float 30. }

(* dist is [0;1[, the relative distance to the end of the wave (we want wave=0 at the end) *)
let wave_strength_at w dist wave =
    let s = sin (wave.length *. dist +. w) in
    s *. s *. wave.strength

let strength_at w dist waves =
    List.fold_left (fun s wave -> wave_strength_at w dist wave +. s) 1. waves

let wind_speed t pos radius now =
    let x = K.to_float pos.(0)
    and y = K.to_float pos.(1) in
    let a, l = ang_norm x y in
    let radius_ratio = l /. radius in
    let x_strength = strength_at now (a /. (2. *. pi)) t.x
    and rad_strength = strength_at now radius_ratio t.rad
    and t_strength = strength_at now ((now -. t.start) /. (t.stop -. t.start)) t.time in
    let n = radius_ratio *. t_strength *. (x_strength +. rad_strength) in
    let make_ang a n =
        [| K.of_float (n *. cos a) ;
           K.of_float (n *. sin a) |] in
    make_ang (a +. 0.5*.pi) n
    (* todo: add small variations in Z *)

