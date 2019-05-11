(* An OCaml implementation of The Captain's Mistress (aka Connect Four) *)
(* Includes various agents for playing the game *)
(* Rules and game processing functions are in game.ml *)
(* Data processing and utility functions are in util.ml *)
(* Search functions and agents are in search.ml *)
(* The play module and functors are in play.ml *)
(* test.ml contains testing functions and should be compiled first *)
(* By Cassandra Bleskachek *)
(* to use, please run '#use "test.ml";;' *)

#use "game.ml"
#use "util.ml"

module type Player =
sig
	val toString: string
	val nextMove: board -> board (* curBoard -> nextBoard *)
end

module RandomPlayer : Player =
struct
	let toString : string = "Random move"

	let nextMove board : board = 
		let player = (if (whatPly board mod 2 = 0) then P1 else P2) 
		in match randomSelect (posMoves player board) with
		| Some x -> x
		| None -> print_endline "RandomPlayer couldn't make a move"; board
end

module Human : Player =
struct

	let toString : string = "Human"

	let rec nextMove board : board =
	(* asks the next move, makes it, and returns it *)
		print_endline "Which column (0-6) would you like to drop in?";
		let ply = whatPly board in
		let player = (if ((ply mod 2) = 0)
						then P1
					else P2)
		in try
			begin
			let x = read_int ()
			in if (x < 0 || x > 6) 
			then begin
				print_endline "Index out of bounds..."; 
				nextMove board
				end
			else match drop player x board with
				| None -> print_endline "That's not a valid move."; nextMove board
				| Some y -> y
			end
		with int_of_string -> print_endline "Not an int..."; nextMove board	
end


(* GREEDY PLAYERS *)

let rec greedySearch board eval : board = 
		let ply = whatPly board in
		let player = (if (ply mod 2 = 0) then P1 else P2) in
		let rec f lst curBestBoard curBestScore = 
			match lst, player with
			| hd::tl, P1 -> begin
								let x = eval hd in
								if (x > curBestScore)
									then f tl hd x
								else f tl curBestBoard curBestScore
							end
			| hd::tl, P2 -> begin
								let x = eval hd in
								if (x < curBestScore)
									then f tl hd x
								else f tl curBestBoard curBestScore
							end
			| [], _ -> curBestBoard
			| _, Blank -> print_endline "greedySearch called with Blank"; newBoard ()
		in f (posMoves player board) (newBoard ()) (if (player = P1) then -200. else 200.)

module GreedyLines : Player =
struct
	let toString : string = "Greedy search using utilLines heuristic"
	let nextMove board : board = greedySearch board utilLines
end

module GreedyDots : Player =
struct
	let toString : string = "Greedy search using utilDots heuristic"
	let nextMove board : board = greedySearch board utilDots
end

module GreedyHeightLines : Player =
struct
	let toString : string = "Greedy search using utilHeightLines heuristic"
	let nextMove board : board = greedySearch board utilHeightLines
end

module GreedyHeightDots : Player =
struct
	let toString : string = "Greedy search using utilHeightDots heuristic"
	let nextMove board : board = greedySearch board utilHeightDots
end

module GreedyWeightLines : Player =
struct
	let toString : string = "Greedy search using utilWeightLines heuristic"
	let nextMove board : board = greedySearch board utilWeightLines
end

module GreedyWeightDots : Player =
struct
	let toString : string = "Greedy search using utilWeightDots heuristic"
	let nextMove board : board = greedySearch board utilWeightDots
end

module GreedyHWLines : Player =
struct
	let toString : string = "Greedy search using utilHWLines heuristic"
	let nextMove board : board = greedySearch board utilHWLines
end

module GreedyHWDots : Player =
struct
	let toString : string = "Greedy search using utilHWDots heuristic"
	let nextMove board : board = greedySearch board utilHWDots
end

module GreedyCowardLines : Player =
struct
	let toString : string = "Greedy search using utilCowardLines heuristic"
	let nextMove board : board = greedySearch board utilCowardLines
end

module GreedyCowardDots : Player =
struct
	let toString : string = "Greedy search using utilCowardDots heuristic"
	let nextMove board : board = greedySearch board utilCowardDots
end


(* MINIMAX PLAYERS *)

let minimax depth board eval : (board * float) =
(* depth-limited negamax *)
		if (depth < 1) then (board, eval board)
		else begin
			let rec mini brd ply dp = 
				begin
					if (dp < 1) then (brd, (eval brd)) (* max depth, run eval function *)
					else let rec f lst curBestBoard curBestScore =
						match lst with
						| hd::[] -> if (curBestScore = 200.)
										then (hd, (eval hd))
									else begin
										let x = snd (maxi hd (ply+1) (dp-1)) 
										in if (x < curBestScore)
											then (hd, x)
										else (curBestBoard, curBestScore)
										end
						| hd::tl ->
							begin
								let y = snd (maxi hd (ply+1) (dp-1))
								in if (y < curBestScore)
									then f tl hd y
								else f tl curBestBoard curBestScore
							end
						| [] -> begin
									if (curBestScore = 200.) 
										then (print_endline "negamax found no moves from this board: ";
											printBoard brd;
											(curBestBoard, curBestScore))
									else (curBestBoard, curBestScore)
								end
					in f (posMoves P2 brd) (newBoard ()) (200.)
				end
			and maxi brd ply dp = 
				begin
					if (dp < 1) then (brd, (eval brd))
					else let rec f lst curBestBoard curBestScore=
						match lst with
						| hd::[] -> if (curBestScore = -200.)
										then let x = eval hd
										in (hd, x)
									else begin
										let x = snd (mini hd (ply+1) (dp-1))
										in if (x > curBestScore)
											then (hd, x)
										else (curBestBoard, curBestScore)
										end
						| hd::tl ->
							begin
								let y = snd (mini hd (ply+1) (dp-1))
								in if (y > curBestScore)
									then f tl hd y
								else f tl curBestBoard curBestScore
							end
						| [] -> begin
									if (curBestScore = -200.) 
										then (print_endline "negamax found no moves from this board: ";
											printBoard brd;
											(curBestBoard, curBestScore))
									else (curBestBoard, curBestScore)
								end
					in f (posMoves P1 brd) (newBoard ()) (-200.)
				end
			in let thisPly = whatPly board
			in if ((thisPly mod 2) = 0)
					then maxi board thisPly depth
				else mini board thisPly depth
			end

module MinimaxLines : Player =
struct
	let toString : string = "Depth-limited minimax search using utilLines heuristic"
	let nextMove board : board = fst (minimax 5 board utilLines)
end

module MinimaxDots : Player =
struct
	let toString : string = "Depth-limited minimax search using utilDots heuristic"
	let nextMove board : board = fst (minimax 5 board utilDots)
end

module MinimaxHeightLines : Player =
struct
	let toString : string = "Depth-limited minimax search using utilHeightLines heuristic"
	let nextMove board : board = fst (minimax 5 board utilHeightLines)
end

module MinimaxHeightDots : Player =
struct
	let toString : string = "Depth-limited minimax search using utilHeightDots heuristic"
	let nextMove board : board = fst (minimax 5 board utilHeightDots)
end

module MinimaxWeightLines : Player =
struct
	let toString : string = "Depth-limited minimax search using utilWeightLines heuristic"
	let nextMove board : board = fst (minimax 5 board utilWeightLines)
end

module MinimaxWeightDots : Player =
struct
	let toString : string = "Depth-limited minimax search using utilWeightDots heuristic"
	let nextMove board : board = fst (minimax 5 board utilWeightDots)
end

module MinimaxHWLines : Player =
struct
	let toString : string = "Depth-limited minimax search using utilHWLines heuristic"
	let nextMove board : board = fst (minimax 5 board utilHWLines)
end

module MinimaxHWDots : Player =
struct
	let toString : string = "Depth-limited minimax search using utilHWDots heuristic"
	let nextMove board : board = fst (minimax 5 board utilHWDots)
end

module MinimaxCowardLines : Player =
struct
	let toString : string = "Depth-limited minimax search using utilCowardLines heuristic"
	let nextMove board : board = fst (minimax 5 board utilCowardLines)
end

module MinimaxCowardDots : Player =
struct
	let toString : string = "Depth-limited minimax search using utilCowardDots heuristic"
	let nextMove board : board = fst (minimax 5 board utilCowardDots)
end


(* BEST AVERAGE PLAYER *)

(*
module Victor : Player =
struct
(* implements the strategic rules outline by Victor Allis for us in VICTOR *)

	type solution = (int * line list)
	(* index in the array and the list of lines "solved" *)

	let toString : string = "VICTOR"

	let rules board : solution list = 
	(* calls each of Allis' rules on the lines where they may be appropriate *)
	(* returns a list of indices and the lines they may solve for the opponent *)
	(* apply these rules in positions where the opponent is to move *)
	(* black can use these rules by default, white must create an odd threat to do so *)
		let claimeven n : solution = 
			if ((n < 0) || (n > 68)) then []
			else 
		in let baseinverse n = 
			if ((n < 0) || (n > 68)) then []
			else 
		in let vertical n = 
			if ((n < 24) || (n > 56)) then []
			else 
(* 		in let aftereven n = 
		in let lowinverse n = 
		in let highinverse n = 
		in let baseclaim n = 
		in let before n = 
		in let specialbefore n =  *)
		in List.concat [claimeven 0; vertical 24]

	let 
end 
*)