(* An OCaml implementation of The Captain's Mistress (aka Connect Four) *)
(* Includes various agents for playing the game *)
(* Rules and game processing functions are in game.ml *)
(* Data processing and utility functions are in util.ml *)
(* Search functions and agents are in search.ml *)
(* The play module and functors are in play.ml *)
(* test.ml contains testing functions and should be compiled first *)
(* By Cassandra Bleskachek *)
(* to use, please run '#use "test.ml";;' *)

open Game
open Search
open Util

module Play (M1 : Player) (M2 : Player) =
struct
	open M1
	open M2

	let thisGame : board list ref = ref ((newBoard ())::[]) (* list of each ply so far*)
	let newGame : unit = thisGame := ((newBoard ())::[]) (* clear game history *)
	let update (board:board) : unit = thisGame := board::!thisGame

	let rec printGame : unit =
		let rec f x =
			match x with
			| hd::tl -> printBoard hd; f tl
			| [] -> ()
		in f (List.rev !thisGame)

	let quickGame () : outcome =
	(* runs a new game; doesn't print the board *)
		print_endline ("quickGame started between" ^ M1.toString ^ " and " ^ M2.toString);
		newGame;
		(* let ply = ref 0 in *)
		let move p b =
			match p with
			| P1 -> M1.nextMove (* !ply *) b
			| P2 -> M2.nextMove (* !ply *) b
			| Blank -> print_endline "error in Play.quickGame.agent"; b
		in let win p b =
			match p with
			| P1 -> print_endline (M1.toString ^ " (P1) has won."); (b, P1)
			| P2 -> print_endline (M2.toString ^ " (P2) has won."); (b, P2)
			| Blank -> print_endline "The game has been drawn."; (b, Blank)
		in let rec f justWent curBoard = (* the game can run 7 ply before one must check for wins *)
			match gameOver curBoard with
			| Some x -> win x curBoard
			| None -> let togo = opponent justWent 
					in let x = move togo curBoard
					in  update x; 
						(* ply := !ply + 1; *)
						f togo x
		in f P2 (newBoard ())

	let gameLoop () : outcome =
	(* runs a new game, printing the board every turn *)
		print_endline ("gameLoop started between" ^ M1.toString ^ " and " ^ M2.toString);
		newGame;
		(* let ply = ref 0 in *)
		let move p b =
			match p with
			| P1 -> M1.nextMove (* !ply *) b
			| P2 -> M2.nextMove (* !ply *) b
			| Blank -> print_endline "error in Play.gameLoop.agent"; b
		in let win p b =
			match p with
			| P1 -> print_endline (M1.toString ^ " (P1) has won."); (b, P1)
			| P2 -> print_endline (M2.toString ^ " (P2) has won."); (b, P2)
			| Blank -> print_endline "The game has been drawn."; (b, Blank)
		in let rec f justWent curBoard = (* the game can run 7 ply before one must check for wins *)
			printBoard curBoard;
			match gameOver curBoard with
			| Some x -> win x curBoard
			| None -> let togo = opponent justWent 
					in let x = move togo curBoard
					in  update x; 
						(* ply := !ply + 1;  *)
						f togo x
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

(* GAME FUNCTORS FOR PAIRS OF PLAYERS *)
module HumanGame = Play(Human)(Human);;
module HumanRandomGame = Play(Human)(RandomPlayer);;
