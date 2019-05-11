module Deprecated =
struct

(* TEST CASES *)
let a : outcome = (*n=1  *) convert "b,b,b,b,b,b,b,b,b,b,b,b,x,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,x,b,b,b,b,b,o,x,o,x,o,o,win"
let b : outcome = (*n=10k*) convert "b,b,b,b,b,b,x,b,b,b,b,b,o,o,x,x,b,b,o,b,b,b,b,b,b,b,b,b,b,b,x,o,b,b,b,b,b,b,b,b,b,b,win"
let c : outcome = (*n=30k*) convert "x,b,b,b,b,b,x,o,x,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,o,o,b,b,b,b,b,b,b,b,b,b,x,o,b,b,b,b,draw"
let d : outcome = (*n=50k*) convert "o,b,b,b,b,b,b,b,b,b,b,b,o,o,x,o,b,b,x,x,b,b,b,b,x,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,win"
let e : outcome = (*n=65k*) convert "o,o,b,b,b,b,x,o,b,b,b,b,x,x,o,x,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,b,loss"

(* UTILITY FUNCTION TESTS *)
let testUtilLines () : unit = 
	print_endline "utilLines: ";
	Printexc.record_backtrace true;
	let f str board = 
		try print_endline (str ^ " value: " ^ (string_of_float (utilLines board)));
		with Invalid_argument s -> print_endline (Printexc.get_backtrace ());
	in f "blank" (newBoard ()); 
	   f "a" (fst a); 
	   f "b" (fst b); 
	   f "c" (fst c); 
	   f "d" (fst d);
	   f "e" (fst e);
	   print_endline ""

let testUtilDots () : unit = 
	print_endline "utilDots: ";
	Printexc.record_backtrace true;
	let f str board =
		try print_endline (str ^ " value: " ^ (string_of_float (utilDots board)));
		with Invalid_argument s -> print_endline (Printexc.get_backtrace ());
 	in f "blank" (newBoard ()); 
 	   f "a" (fst a); 
 	   f "b" (fst b); 
 	   f "c" (fst c); 
 	   f "d" (fst d);
 	   f "e" (fst e);
 	   print_endline ""

let testUtilHeightDots () : unit = 
	print_endline "utilHeightDots: ";
	Printexc.record_backtrace true;
	let f str board =
		try print_endline (str ^ " value: " ^ (string_of_float (utilHeightDots board)));
		with Invalid_argument s -> print_endline (Printexc.get_backtrace ());
 	in f "blank" (newBoard ()); 
 	   f "a" (fst a); 
 	   f "b" (fst b); 
 	   f "c" (fst c); 
 	   f "d" (fst d);
 	   f "e" (fst e);
 	   print_endline ""

 let testUtilWeightDots () : unit = 
	print_endline "utilWeightDots: ";
	Printexc.record_backtrace true;
	let f str board =
		try print_endline (str ^ " value: " ^ (string_of_float (utilWeightDots board)));
		with Invalid_argument s -> print_endline (Printexc.get_backtrace ());
 	in f "blank" (newBoard ()); 
 	   f "a" (fst a); 
 	   f "b" (fst b); 
 	   f "c" (fst c); 
 	   f "d" (fst d);
 	   f "e" (fst e);
 	   print_endline ""

let test () : unit = 
	print_endline "";
	testUtilLines ();
	testUtilDots ();
	testUtilHeightDots ();
	testUtilWeightDots ()

let testUtilSortTromp () : unit =
	let t = ref 0.
	in  print_endline "a (n=1) utilSortTromp: ";
		t := Sys.time ();
		print_endline ("value: " ^ (string_of_float (utilSortTromp (fst a))));
		print_endline ("time: " ^ (string_of_float ((Sys.time ()) -. !t)));
		print_endline "b (n=10k) utilSortTromp: ";
		t := Sys.time ();
		print_endline ("value: " ^ (string_of_float (utilSortTromp (fst b))));
		print_endline ("time: " ^ (string_of_float ((Sys.time ()) -. !t)));
		print_endline "c (n=30k) utilSortTromp: ";
		t := Sys.time ();
		print_endline ("value: " ^ (string_of_float (utilSortTromp (fst c))));
		print_endline ("time: " ^ (string_of_float ((Sys.time ()) -. !t)));
		print_endline "d (n=50k) utilSortTromp: ";
		t := Sys.time ();
		print_endline ("value: " ^ (string_of_float (utilSortTromp (fst d))));
		print_endline ("time: " ^ (string_of_float ((Sys.time ()) -. !t)));
		print_endline "e (n=65k) utilSortTromp: ";
		t := Sys.time ();
		print_endline ("value: " ^ (string_of_float (utilSortTromp (fst e))));
		print_endline ("time: " ^ (string_of_float ((Sys.time ()) -. !t)));
		print_endline "";
;;


let negamax (board:board) (ply:int) : float =
		let rec min b p =
			if hasWon b P1
				then 100.
			else if hasWon b P2
				then -100.
			else begin
					let rec f lst m pl = 
						match lst with
						| hd::tl -> begin
									let y = max hd (pl+1) 
									in if (y < m)
											then f tl y pl
										else f tl m pl
									end 
						| [] -> m
					in f (posMoves P2 b) 200. p
				end (* iterate through posMoves and find the lowest value *)
 		and max b p =
			if hasWon b P1
				then 100.
			else if hasWon b P2
				then -100.
			else begin
					let rec f lst m pl= 
						match lst with
						| hd::tl -> begin
									let y = min hd (pl+1) 
									in if (y > m)
											then f tl y pl
										else f tl m pl
									end 
						| [] -> m
					in f (posMoves P1 b) (-.200.) p
				end (* iterate through posMoves and find the lowest value *)
		in if ((ply mod 2) = 0)
				then max board ply
			else min board ply

module NegamaxEndPlayer : Player =
struct

	let toString : string = "Negamax search to endgame"

	let nextMove board : board = 
		let badScore = 
			match curPlayer (whatPly board) with
			| P1 -> -200.
			| P2 -> 200.
			| Blank -> 0.
		in let f b curScore curBoard = 
			let x = whatPly b 
			in let thisScore = negamax b x 
			in if ((favor (curPlayer x)) thisScore curScore)
				then (thisScore, b)
			else (curScore, curBoard)
		in let rec g lst curScore curBoard = 
			match lst with
			| hd::tl -> let q = f hd curScore curBoard
						in g tl (fst q) (snd q)
			| [] -> curBoard
		in g (posMovesP board) badScore board

end

let negamaxDepth (board:board) (ply:int) (depth:int) util : float =
	(* searches to depth 8, and checks the dataset *)
	(* if deeper than 8, searches to endgame *)
		let rec min b p d =
			if hasWon b P1
				then 100.
			else if hasWon b P2
				then -100.
			else if (d < 1)
				then util b 
			else begin
					let rec f lst m pl = 
						match lst with
						| hd::tl -> begin
									let y = max hd (pl+1) (d-1)
									in if (y < m)
											then f tl y pl
										else f tl m pl
									end 
						| [] -> m
					in f (posMoves P2 b) 200. p
				end (* iterate through posMoves and find the lowest value *)
 		and max b p d =
			if hasWon b P1
				then 100.
			else if hasWon b P2
				then -100.
			else if (d < 1)
				then util b 
			else begin
					let rec f lst m pl = 
						match lst with
						| hd::tl -> begin
									let y = min hd (pl+1) (d-1)
									in if (y > m)
											then f tl y pl
										else f tl m pl
									end 
						| [] -> m
					in f (posMoves P1 b) (-.200.) p
				end (* iterate through posMoves and find the lowest value *)
		in if ((ply mod 2) = 0)
				then max board ply depth
			else min board ply depth

module NegamaxDepthLinesPlayer : Player =
struct

	let toString : string = "Depth-limited negamax using the utilLines"

	let nextMove board : board = 
		let badScore = 
			match curPlayer (whatPly board) with
			| P1 -> -200.
			| P2 -> 200.
			| Blank -> 0.
		in let f b curScore curBoard = 
			let x = whatPly b 
			in let thisScore = negamaxDepth b x 2 utilLines
			in if ((favor (curPlayer x)) thisScore curScore)
				then (thisScore, b)
			else (curScore, curBoard)
		in let rec g lst curScore curBoard = 
			match lst with
			| hd::tl -> let q = f hd curScore curBoard
						in g tl (fst q) (snd q)
			| [] -> curBoard
		in g (posMovesP board) badScore board

end


	(* ORIGINAL VICTORY FUNCTIONS *)

	let posMoves (player:spot) (board:board) : board list =
	(* returns a list of possible boards the player can move to *)
		match player with
		| Blank -> print_endline "Blank cannot move!"; []
		| _ -> let rec f n = 
				begin
				if n > 6 then []
				else match drip player n board with
					| Some b -> b::(f (n+1))
					| None -> f (n+1)
				end;
		in f 0

	let findHorizWin board player : bool =
	(* checks if the player has won horizontally *)
		if player = Blank
			then false
		else let rec g soFar spot =
			if soFar = 4 then true
			(* else if spot > 41 then false (redundant failsafe) *)
			else if (board.(spot) = player)
				then g (soFar+1) (spot+6)
			else false
		in let rec f n =
			if n > 23 (* cut the check short bc there's no space for more wins *)
				then false
			else if (board.(n) = player) 
				then (g 1 (n+6)) || (f (n+1))
			else f (n+1)
		in f 0

	let findVertWin board player : bool =
	(* checks if the player has won vertically *)
		if player = Blank
			then false
		else let rec g soFar spot =
			if soFar = 4 then true
			else if (board.(spot) = player)
				then g (soFar+1) (spot+1)
			else false
		in let rec f n = 
			if n > 41
				then false
			else if (n mod 6) = 3
				then f (n+3) (* reduce checks short bc there's no space for more wins *)
			else if (board.(n) = player)
				then (g 1 (n+1)) || (f (n+1)) 
			else f (n+1)
		in f 0

	let findFSWin board player : bool =
	(* checks if the player has won with a positive slope *)
		if player = Blank
			then false
		else let rec g soFar spot =
			if soFar = 4 then true
			else if (board.(spot) = player)
				then g (soFar+1) (spot+7)
			else false
		in let rec f n =
			if n > 20 (* cut check short bc there's no space for more wins *)
				then false
			else if (n mod 6) = 3
				then f (n+3) (* reduce checks short bc there's no space for more wins *)
			else if (board.(n) = player)
				then (g 1 (n+7)) || (f (n+1))
			else f (n+1)
		in f 0

	let findBSWin board player : bool =
	(* checks if the player has won with a negative slope *)
		if player = Blank
			then false
		else let rec g soFar spot =
			if soFar = 4 then true
			else if (board.(spot) = player)
				then g (soFar+1) (spot-5)
			else false
		in let rec f n =
			if n > 38 (* cut check short bc there's no space for more wins *)
				then false
			else if (n mod 6) = 3
				then f (n+3) (* reduce checks short bc there's no space for more wins *)
			else if (board.(n) = player)
				then (g 1 (n-5)) || (f (n+1))
			else f (n+1)
		in f 18 (* first spot where backslash win is possible *)

	let hasWon board player : bool =
	(* returns whether the given player has won *)
		(findHorizWin board player) || (findVertWin board player) || 
		(findFSWin board player) || (findBSWin board player)

	let hasLost board player : bool =
	(* returns whether the opponent of the given player has won *)
		match player with
		| P1 -> 
			(findHorizWin board P2) || (findVertWin board P2) || 
			(findFSWin board P2) || (findBSWin board P2)
		| P2 -> 
			(findHorizWin board P1) || (findVertWin board P1) || 
			(findFSWin board P1) || (findBSWin board P1)
		| Blank -> false

	let isDraw board : bool =
	(* returns whether the board is full *)
		let rec f n = 
			match board.(n) with 
			| Blank -> false
			| _ -> if (n > 41) then true else f (n+6)
		in ((f 5) && (not (hasWon board P1)) && (not (hasWon board P2)))

	let gameOver board : bool =
	(* returns whether the game is over *)
		(hasWon board P1) || (hasWon board P2) || (isFull board)


	(* FROM THINK: *)

	let trainData : outcome list = List.map convert (lines_from_file "tromp.data")

	let data : outcome array = Array.of_list trainData

	let sortData : outcome array = boardSort data

	let sortedBoardstoFile file : unit = 
	(* saves the sorted baords to a file *)
		let sd = Array.copy sortData in
		let x = open_out file in    (* create or truncate file, return channel *)
			let f o = 
				Printf.fprintf x "%s\n" (convertBack o);   	(* write something *)   
			in Array.iter f sd;
		close_out x; ()               (* flush and close the channel *)
		
	
	let utilTromp board : float = 
	(* searches through Tromp's dataset to return the expected outcome of the board *)
		let rec f lst =
			match lst with
			| (b, w)::tl ->	if (sameBoard board b)
								then begin
									match w with
									| P1 -> 50.
									| P2 -> -50.
									| Blank -> 0.
								end
							else f tl
			| [] -> printBoard board;
					print_endline "board not found in Tromp's dataset."; 0.
		in f trainData

	let utilEndgame board player : float =
	(* searches to endgames to determine a utility value *)
		let rec min b =
			if hasWon board P1
				then 100.
			else if hasWon board P2
				then -100.
			else begin
					let rec f lst m = 
						match lst with
						| hd::tl -> begin
									let y = max hd
									in if (y < m)
											then f tl y
										else f tl m
									end 
						| [] -> m
					in f (posMoves P2 b) 200.
				end (* iterate through posMoves and find the lowest value *)
		and max b =
			if hasWon board P2
				then -100.
			else if hasWon board P1
				then 100.
			else begin
					let rec f lst m = 
						match lst with
						| hd::tl -> begin
									let y = min hd
									in if (y > m)
											then f tl y
										else f tl m
									end 
						| [] -> m
					in f (posMoves P1 b) (-.200.)
				end (* iterate through posMoves and find the lowest value *)
		in match player with
			| P1 -> max board
			| P2 -> min board
			| Blank -> print_endline "utilEndgame called with Blank"; 0.

	
	let negamax (board:board) (ply:int) : float =
	(* searches to depth 8, and checks the dataset *)
	(* if deeper than 8, searches to endgame *)
		let rec min b p =
			if (p = 8)
				then utilTromp board
			else if hasWon board P1
				then 100.
			else if hasWon board P2
				then -100.
			else begin
					let rec f lst m pl = 
						match lst with
						| hd::tl -> begin
									let y = max hd (pl+1) 
									in if (y < m)
											then f tl y pl
										else f tl m pl
									end 
						| [] -> m
					in f (posMoves P2 b) 200. p
				end (* iterate through posMoves and find the lowest value *)
		and max b p =
			if (p = 8)
				then utilTromp board
			else if hasWon board P1
				then 100.
			else if hasWon board P2
				then -100.
			else begin
					let rec f lst m pl= 
						match lst with
						| hd::tl -> begin
									let y = min hd (pl+1) 
									in if (y > m)
											then f tl y pl
										else f tl m pl
									end 
						| [] -> m
					in f (posMoves P1 b) (-.200.) p
				end (* iterate through posMoves and find the lowest value *)
		in if (ply > 8)
			then begin
				if ((ply mod 2) = 0)
					then utilEndgame board P1
				else utilEndgame board P2
				end
			else begin
				if ((ply mod 2) = 0)
					then max board ply
				else min board ply
				end
				
end 
(* type found = 
		| Move of (board * float) (* return best move and utility value *)
		| End of outcome (* game is over, return board and winner *)
		(* | Deep (*max depth reached*) *)

	let foundComp (a:found) (b:found) (player:spot) : int =
		match a, b with
		| Move(_, v1), Move(_, v2) -> if (player = P1)
										then begin
											if (v2 > v1)
												then -1
											else if (v1 > v2)
												then 1
											else 0
											end
									else if (player = P1)
										then begin
											if (v2 > v1)
												then -1
											else if (v1 > v2)
												then 1
											else 0
											end
									else (print_endline "foundComp called w Blank"; 0)
		| Move(_, v), End(_, p) -> if (player = opponent p)
										then 1
									else if (player = p)
										then -1
									else if (player = P1)
										then begin
											if (v > 0.05)
												then 1
											else if (v < 0.05)
												then -1
											else 0
										end
									else if (player = P2)
										then begin
											if (v < 0.)
												then 1
											else if (v > 0.)
												then -1
											else 0
										end
									else (print_endline "foundComp called w Blank"; 0)
	 (* | Deep, End(_, p) -> if (player = opponent p)
								then -1
							else 1
		| Deep, Deep -> 0
		| Deep, _ -> -1 *)
		| End(_, p), Move(_, v) -> if (player = opponent p)
										then -1
									else if (player = p)
										then 1
									else if (player = P1)
										then begin
											if (v > 0.05)
												then -1
											else 1
											end
									else if (player = P2)
										then begin
											if (v < 0.)
												then -1
											else 1
											end
									else (print_endline "foundComp called with Blank"; 0)
		| End (_, p1), End(_, p2) -> if (p2 = p1)
										then 0
									else if (player = p2)
										then -1
									else if (player = p1)
										then 1
									else if ((opponent player) = p2)
										then 1
									else if ((opponent player) = p1)
										then -1
									else (print_endline "error in End, End case in foundComp"; 0)
		(* | End(_, p), _ -> if (player = opponent p)
								then 1
							else -1
		
		| Move(_, v), _ -> -1 *)

	let foundGT a b (player:spot) : bool = 
		match a, b with
		| Move(_, v1), Move(_, v2) -> if (player = P1) then (v1 > v2)
									else if (player = P2) then (v2 > v1)
									else (print_endline "foundComp called w Blank"; false)
		(* | Deep, End(_, p) -> (player = opponent p)
		| Deep, _ -> false *)
		| Move(_, v), End(_, p) -> 	if (player = opponent p) then true
									else if (player = p) then false
									else if (player = P1) then (v > 0.05)
									else if (player = P2) then (v < 0.)
									else (print_endline "foundComp called w Blank"; false)
		| End(_, p), Move(_, v) -> 	if (player = opponent p) then false
									else if (player = p) then true
									else if (player = P1) then (v < 0.05)
									else if (player = P2) then (v > 0.)
									else (print_endline "foundComp called with Blank"; false)
		| End (_, p1), End(_, p2) -> if (player = p1) then true
									else if (player = p2) then false
									else if ((opponent player) = p1) then false
									else if ((opponent player) = p2) then true
									else false
(* 		| End(_, p), _ -> (player = opponent p)
		| Move(_, v), _ -> false *) *)

(* 	let rec negamax player depth board eval : found =
	begin let bestScore = ref (Move ((newBoard ()), 0.))
		in let obs = Move((newBoard ()), 0.)
		in let target = 
		begin match player with
			| P1 -> bestScore := Move (board, -200.); 100.
			| P2 -> bestScore := Move (board, 200.); -100.
			| Blank -> print_endline "negamax called with Blank"; 0.
		end 
		in match depth with
		| 0 -> eval board
		| _ -> 
		begin let bs = ref (Move ((newBoard ()), 0.))
			in let rec f lst = 
				match lst with
				| hd::tl ->
					begin
						if (hasWon hd player)
							then Move (hd, target)
						else 
						begin let thisScore = (negamax (opponent player) (depth-1) hd eval)
							in match opponent player, thisScore, !bs with
							| P1, Move (_,x), Move _ -> 
									if (foundGT thisScore !bs P1)
										then (bs := (hd, x); f tl)
									else f tl
							| P2, Move (_,x), Move _ -> 
									if (foundGT thisScore !bs P2)
										then (bs := (hd, x); f tl)
									else f tl
							| P1, End(_, p), Move _ -> 
									if (P1 = p) 
										then (bs := hd; f tl)
									else f tl
							| P1, Move (_, v), End(_, p) -> 
									if (P2 = p) then (bs := thisScore; f tl)
									else if (foundGT thisScore !bs P1)
										then (bs := (hd, x); f tl)
									else f tl
							| P2, Move (_, v), End(_, p) -> 
									if (P1 = p) then (bs := thisScore; f tl)
									else if (foundGT thisScore !bs P2)
										then f tl
									else (bs := (hd, x); f tl)
							| _, End(_, p1), End(_, p2) -> 
									if (player = p1) then (bs := thisScore; f tl)
									else if (player = p2) then f tl
									else if ((opponent player) = p1) then f tl
									else if ((opponent player) = p2) then (bs := thisScore; f tl)
									else f tl
						 (* | _, Deep, _ -> f tl
							| _, _, Deep -> (bestScore := thisScore; f tl) *)
							| Blank, _, _ -> print_endline "negamax tried to recurse on Blank"; !bs
						end
					end
				| [] -> 
					begin match gameOver board with
						| None -> !bs
						| Some x -> End (board, x)
					end
					(* if (!bs = obs) 
							then (print_endline "negamax found no moves before the game was over"; !bs)
						else !bs  *)
		end
	end
 *)

(* even older stuff below *)


(* An OCaml implementation of The Captain's Mistress (aka Connect Four) *)
(* Includes various agents for playing the game *)
(* By Cassandra Bleskachek *)

	type spot = (* each board position can be blank, x, or o *)
	| Blank (*-*)
	| P1 (*x*)
	| P2 (*o*)

	type board = spot array

module type BoardGame =
sig
	val newBoard: unit -> board
	val printBoard: board -> unit
	val opponent: spot -> spot
	val isFull: board -> bool
	val newGame: unit
	val move: spot -> int -> board -> board option
	val posMoves: spot -> board -> board list
	val hasWon: board -> spot -> bool
	val hasLost: board -> spot -> bool
	val isDraw: board -> bool
	val gameOver: board -> bool
	val printGame: unit
end

module type DataProc =
sig
	type outcome = (board * spot)
	val lines_from_file: string -> string list
	val strlist_to_file: string list -> string -> unit
	val convert: string -> outcome
	val trainData: outcome list
end

module type Player =
sig
	val toString: string
	val setPlayer: spot -> unit
	val whichPlayer: spot
	val nextMove: board -> board
end

module Connect4 : BoardGame =
struct
	(* _____________________
	5 | 5|11|17|23|29|35|41|
	4 | 4|10|16|22|28|34|40|
	3 | 3| 9|15|21|27|33|39|
	2 | 2| 8|14|20|26|32|38|
	1 | 1| 7|13|19|25|31|37|
	0 | 0| 6|12|18|24|30|36|
	   ---------------------
	   	0  1  2  3  4  5  6
	 *)
	let newBoard () : board = Array.make 42 Blank (* starting board *)
	let thisGame : board list ref = ref ((newBoard ())::[]) (* list of each ply so far*)


	(* BOARD INSPECTION FUNCTIONS *)

	let opponent player : spot =
		match player with
		| P1 -> P2
		| P2 -> P1
		| Blank -> Blank

	let arraytoXY index : (int * int) =
	(* returns the (x, y) coordinates of the spot at the Board index *)
		let y = index mod 6 in
		let x = index / 6 in
		(x, y)

	let xytoarray (x:int) (y:int) : int =
	(* returns the array index of the spot at coordinates (x, y) *)
		(6 * x) + y

	let printBoard (board:board) : unit =
		print_endline " _______________";
		for y = 5 downto 0 do
			begin
			print_string " |";
			for x = 0 to 6 do
				begin
					match board.(xytoarray x y) with
					| Blank -> print_string "-|";
					| P1 -> print_string "x|";
					| P2 -> print_string "o|";
				end done;
			print_endline "";
			end done;
		print_endline " ^^^^^^^^^^^^^^^\n"

	let isFull board : bool =
	(* returns whether the board is full *)
		let rec f n = 
			match board.(n) with 
			| Blank -> false
			| _ -> if (n > 41) then true else f (n+6)
		in (f 5)


	(* BOARD MANIPULATION FUNCTIONS *)

	let newGame : unit =
		thisGame := ((newBoard ())::[])

	(* return the effect of as potential move *)
	let drip (player:spot) (column:int) (board:board) : board option =
	(* returns the board produced by player dropping a piece into column, or None *)
	(* DOES NOT update the list of boards so far *)
		let rec f n : board option = 
			begin
			if (not ((n/6) = column)) then None
			else match board.(n) with
				| Blank -> begin
							let x = Array.copy board in
							Array.set x n player; 
							Some x
						   end
				| _ -> f (n+1)
			end
		in f (column * 6)

	(* move with debugging active*)
	let drop (player:spot) (column:int) (board:board) : board option =
	(* returns the board produced by player dropping a piece into column, or None *)
	(* updates the list of boards so far *)
		if (player = Blank) 
			then (print_endline "You're dropping a blank tile..."; None)
		else let rec f n : board option = 
			begin
			if (not ((n/6) = column)) 
				then (print_endline "That column is full!"; None)
			else match board.(n) with
				| Blank -> begin
							let x = Array.copy board in
							Array.set x n player; 
							thisGame := x::!thisGame;
							Some x
						   end
				| _ -> f (n+1)
			end;
		in f (column * 6)

	(* make a move *)
	let move (player:spot) (column:int) (board:board) : board option =
	(* returns the board produced by player dropping a piece into column, or None *)
	(* updates the list of boards so far *)
		let rec f n : board option = 
			begin
			if (not ((n/6) = column)) then None
			else match board.(n) with
				| Blank -> begin
							let x = Array.copy board in
							Array.set x n player; 
							thisGame := x::!thisGame;
							Some x
						   end
				| _ -> f (n+1)
			end;
		in f (column * 6)

	let posMoves (player:spot) (board:board) : board list =
	(* returns a list of possible boards the player can move to *)
		match player with
		| Blank -> print_endline "Blank cannot move!"; []
		| _ -> let rec f n = 
				begin
				if n > 6 then []
				else match drip player n board with
					| Some b -> b::(f (n+1))
					| None -> f (n+1)
				end;
		in f 0


	(* VICTORY FUNCTIONS *)

	let findHorizWin board player : bool =
	(* checks if the player has won horizontally *)
		if player = Blank
			then false
		else let rec g soFar spot =
			if soFar = 4 then true
			(* else if spot > 41 then false (redundant failsafe) *)
			else if (board.(spot) = player)
				then g (soFar+1) (spot+6)
			else false
		in let rec f n =
			if n > 23 (* cut the check short bc there's no space for more wins *)
				then false
			else if (board.(n) = player) 
				then (g 1 (n+6)) || (f (n+1))
			else f (n+1)
		in f 0

	let findVertWin board player : bool =
	(* checks if the player has won vertically *)
		if player = Blank
			then false
		else let rec g soFar spot =
			if soFar = 4 then true
			else if (board.(spot) = player)
				then g (soFar+1) (spot+1)
			else false
		in let rec f n = 
			if n > 41
				then false
			else if (n mod 6) = 3
				then f (n+3) (* reduce checks short bc there's no space for more wins *)
			else if (board.(n) = player)
				then (g 1 (n+1)) || (f (n+1)) 
			else f (n+1)
		in f 0

	let findFSWin board player : bool =
	(* checks if the player has won with a positive slope *)
		if player = Blank
			then false
		else let rec g soFar spot =
			if soFar = 4 then true
			else if (board.(spot) = player)
				then g (soFar+1) (spot+7)
			else false
		in let rec f n =
			if n > 20 (* cut check short bc there's no space for more wins *)
				then false
			else if (n mod 6) = 3
				then f (n+3) (* reduce checks short bc there's no space for more wins *)
			else if (board.(n) = player)
				then (g 1 (n+7)) || (f (n+1))
			else f (n+1)
		in f 0

	let findBSWin board player : bool =
	(* checks if the player has won with a negative slope *)
		if player = Blank
			then false
		else let rec g soFar spot =
			if soFar = 4 then true
			else if (board.(spot) = player)
				then g (soFar+1) (spot-5)
			else false
		in let rec f n =
			if n > 38 (* cut check short bc there's no space for more wins *)
				then false
			else if (n mod 6) = 3
				then f (n+3) (* reduce checks short bc there's no space for more wins *)
			else if (board.(n) = player)
				then (g 1 (n-5)) || (f (n+1))
			else f (n+1)
		in f 18 (* first spot where backslash win is possible *)

	let hasWon board player : bool =
	(* returns whether the given player has won *)
		(findHorizWin board player) || (findVertWin board player) || 
		(findFSWin board player) || (findBSWin board player)

	let hasLost board player : bool =
	(* returns whether the opponent of the given player has won *)
		match player with
		| P1 -> 
			(findHorizWin board P2) || (findVertWin board P2) || 
			(findFSWin board P2) || (findBSWin board P2)
		| P2 -> 
			(findHorizWin board P1) || (findVertWin board P1) || 
			(findFSWin board P1) || (findBSWin board P1)
		| Blank -> false

	let isDraw board : bool =
	(* returns whether the board is full *)
		let rec f n = 
			match board.(n) with 
			| Blank -> false
			| _ -> if (n > 41) then true else f (n+6)
		in ((f 5) && (not (hasWon board P1)) && (not (hasWon board P2)))

	let gameOver board : bool =
	(* returns whether the game is over *)
		(hasWon board P1) || (hasWon board P2) || (isFull board)

	let rec printGame : unit =
		let rec f x =
			match x with
			| hd::tl -> printBoard hd; f tl
			| [] -> ()
		in f (List.rev !thisGame)
end


module Data : DataProc =
struct
	open Connect4
	type outcome = (board * spot) (* board and the expected winner *)

	(* Load a list of strings from a text file. Not particularly efficient
	   as it is not tail recursive but gets the job done. *)
	let lines_from_file filename =
	  let rec helper inchan =
	    try
	      let line = input_line inchan in
	      line :: (helper inchan)
	    with
	    | End_of_file ->
	       close_in inchan;
	       []
	  in
	  let inchan = open_in filename in
	  helper inchan

	(* Write a list of strings to file filename. Tail recursive so
	   reasonably efficient. *)
	let strlist_to_file strlist filename  =
	  let rec helper outchan list =
	    if list = [] then
	      close_out outchan
	    else
	      begin
	        output_string outchan (List.hd list);
	        output_string outchan "\n";
	        helper outchan (List.tl list);
	      end
	  in
	  let outchan = open_out filename in
	  helper outchan strlist

	let convert str : outcome =
	  let charlst = String.split_on_char ',' str in
	  let rec read lst i arr = 
	    match lst with
	    | "x"::tl -> arr.(i) <- P1; read tl (i+1) arr
	    | "o"::tl -> arr.(i) <- P2; read tl (i+1) arr
	    | "b"::tl -> arr.(i) <- Blank; read tl (i+1) arr
	    | "win"::tl -> (arr, P1)
	    | "loss"::tl -> (arr, P2)
	    | "draw"::tl -> (arr, Blank)
	    | _ -> print_endline "trainData read unknown things"; (Connect4.newBoard (), Blank)
	  in read charlst 0 (Connect4.newBoard ())

	let trainData : outcome list = List.map convert (lines_from_file "connect-4.data")
end


module Human : Player =
struct
	open Connect4
	open Data
	
	let playernumber : spot ref = ref Blank

	let toString : string = "Human"

	let setPlayer spot : unit =
	(* sets the value of this player *)
		match spot with
		| P1 -> playernumber := P1
		| P2 -> playernumber := P2
		| Blank -> ()

	let whichPlayer : spot = !playernumber
	(* returns which player spot this agent has*)

	let rec nextMove board : board =
	(* asks the next move, makes it, and returns it *)
		print_endline "Which column (0-6) would you like to drop in?";
		let x = read_int () in
		match move whichPlayer x board with
		| None -> print_endline "That's not a valid move."; nextMove board
		| Some y -> y
end

module Play (M1 : Player) (M2 : Player) =
struct
	open Connect4
	open Data
	open M1
	open M2
	type outcome = Data.outcome

	let quickGame () : outcome =
	(* runs a new game; doesn't print the board; doesn't check for wins until needed *)
		print_endline "quickGame started";
		M1.setPlayer P1;
		M2.setPlayer P2;
		newGame;
		let agent x =
			match x with
			| P1 -> M1.nextMove
			| P2 -> M2.nextMove
			| Blank -> print_endline "error in Play.quickGame.agent"; Human.nextMove
		in let win p b =
			match p with
			| P1 -> print_endline (M1.toString ^ " (P1) has won."); (b, P1)
			| P2 -> print_endline (M2.toString ^ " (P2) has won."); (b, P2)
			| Blank -> print_endline "error in Play.quickGame.win"; (b, Blank)
		in let ply4done = M2.nextMove (M1.nextMove (M2.nextMove (M1.nextMove (newBoard ())))) 
		in let ply7done = M1.nextMove (M2.nextMove (M1.nextMove ply4done))
		in let rec f justWent curBoard = (* the game can run 7 ply before one must check for wins *)
			if hasWon curBoard justWent then win justWent curBoard
			else if isFull curBoard then (print_endline "The game has been drawn."; (curBoard, Blank))
			else let togo = opponent justWent 
			in f togo ((agent togo) curBoard)
		in f P1 ply7done

	let gameLoop () : outcome =
	(* runs a new game, printing the board every turn *)
		M1.setPlayer P1;
		M2.setPlayer P2;
		newGame;
		let agent x =
			match x with
			| P1 -> M1.nextMove
			| P2 -> M2.nextMove
			| Blank -> print_endline "error in Play.gameLoop.agent"; Human.nextMove
		in let win p b =
			match p with
			| P1 -> print_endline (M1.toString ^ " (P1) has won."); (b, P1)
			| P2 -> print_endline (M2.toString ^ " (P2) has won."); (b, P2)
			| Blank -> print_endline "error in Play.gameLoop.win"; (b, Blank)
		in let rec f justWent curBoard = 
			printBoard curBoard;
			if hasWon curBoard justWent then win justWent curBoard
			else if isFull curBoard then (print_endline "The game has been drawn."; (curBoard, Blank))
			else let togo = opponent justWent 
			in f togo ((agent togo) curBoard)
		in f P2 (newBoard ())

	let tournament () : unit =
	(* runs 10 games with AB order, then 10 with BA order, then prints results *) 
		let rec f n soFar =
			if n > 9 
				then soFar
			else f (n+1) ((quickGame ())::soFar)
		in let rec g n soFar =
			if n > 9
				then soFar
			else g (n+1) ((quickGame ())::soFar)
		in let rec champ p1 p2 d lst =
			match lst with
			| hd::tl -> begin
						match snd hd with
						| P1 -> champ (p1+1) p2 d tl
						| P2 -> champ p1 (p2+1) d tl
						| Blank -> champ p1 p2 (d+1) tl
						end
			| [] -> (string_of_int p1)^"-"^(string_of_int p2)^"-"^(string_of_int d)
		in let ab = champ 0 0 0 (f 0 [])
		in let ba = champ 0 0 0 (g 0 [])
		in print_endline ((M1.toString)^"-"^(M2.toString)^" record was "^ab);
		print_endline ((M2.toString)^"-"^(M1.toString)^" record was "^ba)

	let tournaPrint () : unit = 
	(* same as tournament, but runs gameLoop instead of quickGame *)
		let rec f n soFar =
			if n > 9 
				then soFar
			else f (n+1) ((gameLoop ())::soFar)
		in let rec g n soFar =
			if n > 9
				then soFar
			else g (n+1) ((gameLoop ())::soFar)
		in let rec champ p1 p2 d lst =
			match lst with
			| hd::tl -> begin
						match snd hd with
						| P1 -> champ (p1+1) p2 d tl
						| P2 -> champ p1 (p2+1) d tl
						| Blank -> champ p1 p2 (d+1) tl
						end
			| [] -> (string_of_int p1)^"-"^(string_of_int p2)^"-"^(string_of_int d)
		in let ab = champ 0 0 0 (f 0 [])
		in let ba = champ 0 0 0 (g 0 [])
		in print_endline ((M1.toString)^"-"^(M2.toString)^" record was "^ab);
		print_endline ((M2.toString)^"-"^(M1.toString)^" record was "^ba)
end
	
	module HumanGame = Play(Human)(Human);;


	(* _____________________
	5 | 5|11|17|23|29|35|41|
	4 | 4|10|16|22|28|34|40|
	3 | 3| 9|15|21|27|33|39|
	2 | 2| 8|14|20|26|32|38|
	1 | 1| 7|13|19|25|31|37|
	0 | 0| 6|12|18|24|30|36|
	   ---------------------
	   	0  1  2  3  4  5  6
	 *)

(* 	let utilOpenSpots board : float =
		let open1 x y player : float =
		    if (board.(xytoarray x y) = Blank) 
		    	then 0.
			else begin
				(if ((board.(xytoarray x y) = player) 
					&& (board.(xytoarray (x-1) y) = Blank)
					&& (board.(xytoarray (x+1) y) = Blank))
						then 1.
				else 0.) +.
				(if ((board.(xytoarray x y) = player) 
					&& (board.(xytoarray x (y-1)) = Blank)
					&& (board.(xytoarray x (y+1)) = Blank))
						then 1.
				else 0.) +. 
				(if ((board.(xytoarray x y) = player) 
					&& (board.(xytoarray (x-1) (y-1)) = Blank)
					&& (board.(xytoarray (x+1) (y+1)) = Blank))
						then 1.
				else 0.) +.
				(if ((board.(xytoarray x y) = player) 
					&& (board.(xytoarray (x-1) (y+1)) = Blank)
					&& (board.(xytoarray (x+1) (y-1)) = Blank))
						then 1.
				else 0.)
				end
		in let half_open2 x y player : float = 
		 *)


	let testUtilTrompUnsorted () : unit =
		let t = ref 0.
		in  print_endline "n=1 utilTromp: ";
			t := Sys.time ();
			print_endline ("value: " ^ (string_of_float (Think.utilTromp (fst a))));
			print_endline ("time: " ^ (string_of_float ((Sys.time ()) -. !t)));
			print_endline "n=10k utilTromp: ";
			t := Sys.time ();
			print_endline ("value: " ^ (string_of_float (Think.utilTromp (fst b))));
			print_endline ("time: " ^ (string_of_float ((Sys.time ()) -. !t)));
			print_endline "n=30k utilTromp: ";
			t := Sys.time ();
			print_endline ("value: " ^ (string_of_float (Think.utilTromp (fst c))));
			print_endline ("time: " ^ (string_of_float ((Sys.time ()) -. !t)));
			print_endline "n=50k utilTromp: ";
			t := Sys.time ();
			print_endline ("value: " ^ (string_of_float (Think.utilTromp (fst d))));
			print_endline ("time: " ^ (string_of_float ((Sys.time ()) -. !t)));
			print_endline "n=65k utilTromp: ";
			t := Sys.time ();
			print_endline ("value: " ^ (string_of_float (Think.utilTromp (fst e))));
			print_endline ("time: " ^ (string_of_float ((Sys.time ()) -. !t)));
			print_endline "";
	;;