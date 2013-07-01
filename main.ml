let clock =
    let last = ref None in
    fun () ->
        let now = Unix.gettimeofday () in
        let dt = match !last with
            | None -> 0.
            | Some t -> now -. t in
        last := Some now ;
        now, dt

let () =
	(* Init random number generator without seed *)
	Random.self_init () ;
	Arg.parse
		[ "-seed", Arg.Int Random.init, "Use this seed to init random generator" ]
		(fun p -> failwith ("Unknown parameter '"^p))
		"MLRocket v0, a space exploration program\nOptions :" ;
    let now, _dt = clock () in
	let world = World.make ~radius:150 now in
	Game.play world clock
