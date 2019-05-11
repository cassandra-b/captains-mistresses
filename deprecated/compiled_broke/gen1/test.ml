(* An OCaml implementation of The Captain's Mistress (aka Connect Four) *)
(* Includes various agents for playing the game *)
(* Rules and game processing functions are in game.ml *)
(* Data processing and utility functions are in util.ml *)
(* Search functions and agents are in search.ml *)
(* The play module and functors are in play.ml *)
(* test.ml contains testing functions and should be compiled first *)
(* By Cassandra Bleskachek *)

#use "game.ml"
#use "util.ml"
#use "search.ml"
#use "play.ml"

let filename = "results2.txt"

(* TEST CASES *)
let a : outcome = (*n=1  *) convert "b,b,b,b,b,b,b,b,b,b,b,b,x,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,x,b,b,b,b,b,o,x,o,x,o,o,win"
let b : outcome = (*n=10k*) convert "b,b,b,b,b,b,x,b,b,b,b,b,o,o,x,x,b,b,o,b,b,b,b,b,b,b,b,b,b,b,x,o,b,b,b,b,b,b,b,b,b,b,win"
let c : outcome = (*n=30k*) convert "x,b,b,b,b,b,x,o,x,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,o,o,b,b,b,b,b,b,b,b,b,b,x,o,b,b,b,b,draw"
let d : outcome = (*n=50k*) convert "o,b,b,b,b,b,b,b,b,b,b,b,o,o,x,o,b,b,x,x,b,b,b,b,x,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,win"
let e : outcome = (*n=65k*) convert "o,o,b,b,b,b,x,o,b,b,b,b,x,x,o,x,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,loss"

let testUtility util str : unit = 
(* called with an evaluation function and a string description of it *)
	add_to_file ("evaluation function: " ^ str ^ "\n") filename;
	let f str board = 
		try add_to_file (str ^ " value: " ^ (string_of_float (util board)) ^ "\n") filename;
		with Invalid_argument s -> add_to_file (Printexc.get_backtrace ()) filename;
	in f "blank" (newBoard ()); 
	   f "a  1 " (fst a); 
	   f "b 10k" (fst b); 
	   f "c 30k" (fst c); 
	   f "d 50k" (fst d);
	   f "e 65k" (fst e);
	   add_to_file "\n\n" filename

module Test (Agent : Player) = 
struct
	open Agent
	module Game1 = Play(Agent)(RandomPlayer)
	module Game2 = Play(RandomPlayer)(Agent)


	let vsRandom () : unit = 
		let avgTime = ref 0.
		in let wins = ref 0
		in let losses = ref 0
		in let draws = ref 0
		in let x = (Printf.sprintf "Testing for %s vs. RandomPlayer:\n" Agent.toString)
		in add_to_file x filename;
		let rec iter n =
			if (n > 30) 
				then ()
			else let t = Sys.time ()
				in let oc = Game1.quickGame ()
				in let time = (Sys.time ()) -. t
				in avgTime := !avgTime +. time; 
				add_to_file (Printf.sprintf "Game #%02d time: %6.8f \noutcome: %s\n" n time (convertBack oc)) filename;
				(match snd oc with
				| P1 -> wins := !wins + 1
				| P2 -> losses := !losses + 1
				| Blank -> draws := !draws + 1);
				iter (n+1)
		in iter 1;
		add_to_file (Printf.sprintf "average time: %6.8f \n" (!avgTime /. 30.)) filename;
		add_to_file (Printf.sprintf "%s first-player win-rate: %d-%d-%d\n" Agent.toString (!wins) (!losses) (!draws)) filename;
		wins := 0; losses := 0; draws := 0; avgTime := 0.;

		let y = (Printf.sprintf "\nTesting for RandomPlayer vs. %s:\n" Agent.toString)
		in add_to_file y filename;
		let rec itr n =
			if (n > 30) 
				then ()
			else let t = Sys.time ()
				in let oc = Game2.quickGame ()
				in let time = (Sys.time ()) -. t
				in avgTime := !avgTime +. time; 
				add_to_file (Printf.sprintf "Game #%02d time: %6.8f \noutcome: %s\n" n time (convertBack oc)) filename;
				(match snd oc with
				| P2 -> wins := !wins + 1
				| P1 -> losses := !losses + 1
				| Blank -> draws := !draws + 1);
				itr (n+1)
		in itr 1;
		add_to_file (Printf.sprintf "average time: %6.8f \n" (!avgTime /. 30.)) filename;
		add_to_file (Printf.sprintf "%s second-player win-rate: %d-%d-%d\n\n\n" Agent.toString (!wins) (!losses) (!draws)) filename;
end

let originalTests () = 
	add_to_file begin
		"evaluation function: utilSortTromp\n" ^ 
		"blank value: None\n" ^
		"a  1  value: 50.\n" ^
		"b 10k value: 50.\n" ^
		"c 30k value: 0.\n" ^
		"d 50k value: 50.\n" ^
		"e 65k value: -50.\n\n\n"
				end filename;
	testUtility utilLines "utilLines";
	testUtility utilDots "utilDots";
	testUtility utilHeightLines "utilHeightLines";
	testUtility utilHeightDots "utilHeightDots";
	testUtility utilWeightLines "utilWeightLines";
	testUtility utilWeightDots "utilWeightDots";
	testUtility utilHWLines "utilHWLines";
	testUtility utilHWDots "utilHWDots";
	testUtility utilCowardLines "utilCowardLines";
	testUtility utilCowardDots "utilCowardDots";

	let module A = Test(GreedyLines) in A.vsRandom ();
	let module B = Test(GreedyDots) in B.vsRandom ();
	let module C = Test(GreedyHeightLines) in C.vsRandom ();
	let module D = Test(GreedyHeightDots) in D.vsRandom ();
	let module E = Test(GreedyWeightLines) in E.vsRandom ();
	let module F = Test(GreedyWeightDots) in F.vsRandom ();
	let module G = Test(GreedyHWLines) in G.vsRandom ();
	let module H = Test(GreedyHWDots) in H.vsRandom ();
	let module I = Test(GreedyCowardLines) in I.vsRandom ();
	let module J = Test(GreedyCowardDots) in J.vsRandom ();

	let module K = Test(MinimaxLines) in K.vsRandom ();
	let module L = Test(MinimaxDots) in L.vsRandom ();
	let module M = Test(MinimaxHeightLines) in M.vsRandom ();
	let module N = Test(MinimaxHeightDots) in N.vsRandom ();
	let module O = Test(MinimaxWeightLines) in O.vsRandom ();
	let module P = Test(MinimaxWeightDots) in P.vsRandom ();
	let module Q = Test(MinimaxHWLines) in Q.vsRandom ();
	let module R = Test(MinimaxHWDots) in R.vsRandom ();
	let module S = Test(MinimaxCowardLines) in S.vsRandom ();
	let module T = Test(MinimaxCowardDots) in T.vsRandom ();