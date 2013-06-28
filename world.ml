open Mlrocket

type t =
	{ ground : Path.t ;
	  rockets : Rocket.t list ;
      mutable sparkles : Sparkle.t list ;
      mutable ignition : (int * Rocket.t * G.V.t * K.t) option ;
	  gravity : K.t	;
      radius : K.t ;
      mutable max_speed : K.t }

let randomize_path res path =
	mlog "\t\tRandomizing ground..." ;
	let path' = ref None in
	let last_ctrl = ref Point.zero in
	let last_p = ref Point.zero in
	let add_segment p =
		let r = (K.to_float res) *. 1. in
		let half_r = r /. 2. in
		let rand () = K.of_float ((Random.float r) -. half_r) in
		let rand_dec = [| rand () ; rand () |] in
		match !path' with
		| None ->
			path' := Some (Path.empty p) ;
			last_ctrl := rand_dec ;
			last_p := p ;
		| Some path'' ->
            let right_turn v = [| v.(1) ; K.neg v.(0) |] in
			let rand_pt = right_turn (G.V.add rand_dec (G.V.sub p !last_p)) in
			path' := Some (Path.extend path'' p [Point.sub !last_p !last_ctrl ; Point.add p rand_pt] Path.make_bezier_curve) ;
			last_ctrl := rand_pt ;
			last_p := p in
	Path.iter res path add_segment ;
	Bricabrac.unopt !path'

(* Note : the space is incurved the other way around, so that the ground surrounds us. *)
let make_ground ~radius =
	mlog "\tBuilding ground..." ;
	(* Start from a mere "sphere" *)
	let up    = Point.make_unit 1 in
	let down  = Point.sub Point.zero up in
	let right = Point.make_unit 0 in
	let left  = Point.sub Point.zero right in
	let upper_right  = Point.add right up in
	let bottom_right = Point.add right down in
	let upper_left   = Point.add left  up in
	let bottom_left  = Point.add left  down in
	let circ_0 = Path.extend (Path.empty left) up    [ upper_left ]   Path.make_bezier_curve in
	let circ_1 = Path.extend circ_0            right [ upper_right ]  Path.make_bezier_curve in
	let circ_2 = Path.extend circ_1            down  [ bottom_right ] Path.make_bezier_curve in
	let circle = Path.extend circ_2            left  [ bottom_left ]  Path.make_bezier_curve in
	let grnd_0 = Path.scale circle Point.zero radius in
	let grnd_1 = randomize_path (K.of_float 9.) grnd_0 in
	let ground = randomize_path (K.of_float 3.) grnd_1 in
	ground
	
let make ~radius =
    let radius = K.of_int radius in
	mlog "Building world of radius %a..." K.print radius ;
	let gravity = K.of_float 0.1 in
	mlog "\tGravity = %a" K.print gravity ;
	{ ground = make_ground ~radius ;
	  rockets = [ Rocket.make (Point.mul (K.half radius) (Point.make_unit 0)) ] ;
      sparkles = [] ;
      ignition = None ;
	  gravity ;
      radius ;
      max_speed = K.zero }

let prec = K.of_float 1.

let run dt world =
    List.iter (fun rocket ->
        (* move rocket *)
        Rocket.run world.gravity dt rocket ;
        (* check collision with ground *)
        let m = View.get_transform ~src:(Rocket.viewable rocket) () in
        if Poly.exists (fun p ->
            let p' = G.M.mul_vec m [| p.(0) ; p.(1) ; K.one ; K.one |] in
            not (Path.is_inside prec world.ground p'))
            (Rocket.poly rocket)
        then (
            mlog "boum!" ;
            mlog "Your max speed was %a" K.print (K.sqrt world.max_speed) ;
            exit 0
        ) else (
            (* record max speed *)
            let speed = G.V.norm2 (Rocket.speed rocket) in
            if K.compare speed world.max_speed > 0 then world.max_speed <- speed
        ))
        world.rockets ;
    (* create new sparkles *)
    let rec new_sparkles pos s_orient s_thrust prev = function
        | 0 -> prev
        | n ->
            let prev = Sparkle.make pos s_orient 0.2 s_thrust :: prev in
            new_sparkles pos s_orient s_thrust prev (n-1) in
    world.ignition <- Bricabrac.optbind world.ignition (fun (n, rocket, s_orient, s_thrust) ->
        let ignite_ratio = 5 in (* spat 1/5 of n at every 50th of second *)
        let nn = (K.to_int (K.mul (K.mul dt (K.of_int 50)) (K.of_int n))) / ignite_ratio in
        let nn = if nn = 0 then n else nn in
        let n = n - nn in
        let pos = Rocket.pos rocket in
        let s_pos = G.V.add pos s_orient in
        world.sparkles <- new_sparkles s_pos s_orient s_thrust world.sparkles nn ;
        if n < 1 then None
        else Some (n, rocket, s_orient, s_thrust)) ;
    (* animate sparkles while eliminating old ones *)
    world.sparkles <- List.filter (fun sparkle ->
        (* move *)
        Sparkle.run world.gravity dt sparkle ;
        if not (Path.is_inside prec world.ground sparkle.Sparkle.pos) then (
            (* reflect the sparkle *)
            for c = 0 to 1 do
                sparkle.Sparkle.speed.(c) <-
                    let x = K.half (K.neg sparkle.Sparkle.speed.(c)) in
                    K.add x (K.of_float (Random.float 60. -. 30.))
            done ;
        ) ;
        K.compare sparkle.Sparkle.life K.zero > 0)
        world.sparkles

