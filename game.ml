open Mlrocket

let game_clic world camera = function
    | G.Clic (x, y, w, h) ->
        let m = View.get_transform ~dst:camera () in
        let m = G.M.mul_mat (G.get_projection ()) m in
        let clickpos = G.unproject (0,0,w,h) m x (h-y) in
        let rocket = List.hd world.World.rockets in
        let pos = Rocket.pos rocket in
        let pos' = Point.sub clickpos pos in
        let n = Point.norm pos' in
        let orient = Point.mul (K.inv n) pos' in
        Rocket.set_orient rocket orient ;
        (* Are we allowed to use thrust? *)
        (match world.World.ignition with
        | Some _ -> () (* nope *)
        | None ->
            let n = K.min n (K.of_int 20) in
            Rocket.set_thrust rocket (K.mul n (K.of_float 0.03)) ;
            (* spawn new sparkles *)
            let s_orient = Array.map K.neg orient in   (* oposite direction *)
            let s_thrust = K.mul n (K.of_float 6.) in
            let nb = int_of_float (K.to_float n *. 2.) in
            world.World.ignition <- Some (nb, rocket, s_orient, s_thrust))
    | _ -> ()

let game_painter camera () =
	View.draw_viewable camera

let pos_of_camera world =
	let rocket = List.hd world.World.rockets in
    Rocket.pos rocket

let speed_of_camera world =
	let rocket = List.hd world.World.rockets in
    G.V.norm (Rocket.speed rocket)

let zoom_of_camera =
	let speed_to_zoom = K.of_float 50. in
	let min_zoom = K.of_float 10. and max_zoom = K.of_float 30. in
	fun world ->
        let speed = speed_of_camera world in
		let zoom = K.add (K.mul speed speed_to_zoom) min_zoom in
		K.min max_zoom zoom

let orient_of_camera world =
	let rocket = List.hd world.World.rockets in
	let pos = Rocket.pos rocket in
	let n = Point.norm pos in
	(* we are not supposed to reach that far, but that's the starting position for now :-) *)
	if K.to_float n > 0.1 then
        Point.mul (K.inv n) [| K.neg pos.(1) ; pos.(0) |]
    else
        [| K.one ; K.zero |]

let clock_dt =
    let last = ref None in
    fun () ->
        let n = Unix.gettimeofday () in
        let dt = match !last with
            | None -> 0.01
            | Some t -> n -. t in
        last := Some n ;
        dt

let camera_of_world world =
	let root =
		let bg = Pic.Clear, Pic.uni_gc [| K.of_float 0.08 ; K.of_float 0.08 ; K.of_float 0.18 |] in
		let stars = 
			let rec add_star l n =
                if n = 0 then l else
				let rand () = K.sub (K.of_float (Random.float (K.to_float (K.double world.World.radius)))) world.World.radius in
				let point =
					let rad2 = K.square world.World.radius in
					let rec aux () =
						let p = [| rand () ; rand () |] in
						if K.compare (Point.norm2 p) rad2 < 0 then p
						else aux () in
					aux () in
				let col = Pic.rand_col () in
				let star = Pic.Dot point, Pic.uni_gc col in
				add_star (star::l) (n-1) in
			let stars_density = 0.01 in
			let world_surface = K.to_float (K.mul pi (K.square world.World.radius)) in
			let nb_stars = int_of_float (stars_density *. world_surface) in
			mlog "\tAdding %d stars..." nb_stars ;
			add_star [] nb_stars in
		View.make_viewable "root" (fun () ->
            let p00,p10,p11,p01 = View.clip_coordinates () in
            let bbox = Bbox.(add (add (add (make p00) p10) p11) p01) in
            let reclip f paths =
                List.flatten (List.map f paths) in
            let g = reclip (Path.clip p00 p10) [world.World.ground] in
            let g = reclip (Path.clip p10 p11) g in
            let g = reclip (Path.clip p11 p01) g in
            let g = reclip (Path.clip p01 p00) g in
            let gc = Pic.uni_gc [| K.one ; K.one ; K.one |] in
            let grounds = List.map (fun p -> Pic.Path p, gc) g in
            let sparkles = List.map (fun s -> Pic.Dot s.Sparkle.pos, s.Sparkle.gc) world.World.sparkles in
            let flowers = List.flatten (List.map (fun f ->
                if Bbox.intersect bbox f.Flower.bbox then
                    List.map (fun p -> Pic.Path p, f.Flower.gc) f.Flower.paths
                else [])
                world.World.flowers) in
            Pic.draw ~prec:World.prec (bg :: grounds @ sparkles @ stars) ;
            Pic.draw ~prec:(K.of_float 0.5) flowers) View.identity in
	List.iter
		(fun rocket ->
			Rocket.set_viewable rocket (View.make_viewable
				~parent:root "a rocket"
				(fun () ->
                    Pic.draw [ Pic.Poly (Rocket.poly rocket), Pic.uni_gc [| K.one ; K.one ; K.one |] ])
				(View.trans_orientor
                    (fun () ->
                        let pos = Rocket.pos rocket in
                        pos.(0), pos.(1), K.zero)
                    (fun () ->
                        let o = Rocket.orient rocket in
                        o.(0), o.(1)))))
		world.World.rockets ;
	let rocket_follower = View.make_viewable
		~parent:root "rocket follower"
		(fun () -> ())
		(View.trans_orientor
			(fun () ->
				let pos = pos_of_camera world in
				pos.(0), pos.(1), K.one)
			(fun () ->
                let o = orient_of_camera world in
                o.(0), o.(1))) in
	View.make_viewable
		~parent:rocket_follower "camera"
		(fun () -> ())
		(View.scaler
			(fun () -> let zoom = zoom_of_camera world in zoom, zoom, K.one))

let play world =
	let camera = camera_of_world world in
	View.display
		~on_event:(game_clic world camera)
        [ (fun () ->
            let dt = clock_dt () in
            World.run (K.of_float dt) world) ;
		  game_painter camera ]

