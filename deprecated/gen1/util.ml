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
	
	(* TROMP DATA PROCESSING FUNCTIONS *)

	(* Load a list of strings from a text file. Not particularly efficient
	   as it is not tail recursive but gets the job done. *)
let lines_from_file filename =
	let rec helper inchan =
    	try let line = input_line inchan 
    		in (line :: (helper inchan))
	    with
	    	| End_of_file -> close_in inchan; []
	in let inchan = open_in filename 
	in helper inchan

	(* Write a list of strings to file filename. Tail recursive so
	   reasonably efficient. *)
let strlist_to_file strlist filename  =
	let rec helper outchan lst =
	    if (lst = []) 
	    	then close_out outchan
	    else begin
	        output_string outchan (List.hd lst);
	        output_string outchan "\n";
	        helper outchan (List.tl lst);
	    	end
	in let outchan = open_out filename 
	in helper outchan strlist

let add_to_file str filename : unit = 
	let out = open_out_gen (Open_append::[]) 0o644 filename
		in output_string out str;
		close_out out

let convert str : outcome =
	let charlst = String.split_on_char ',' str 
	in let rec read lst i arr = 
		match lst with
	    | "x"::tl -> arr.(i) <- P1; read tl (i+1) arr
	    | "o"::tl -> arr.(i) <- P2; read tl (i+1) arr
	    | "b"::tl -> arr.(i) <- Blank; read tl (i+1) arr
	    | "win"::tl -> (arr, P1)
	    | "loss"::tl -> (arr, P2)
	    | "draw"::tl -> (arr, Blank)
	    | _ -> print_endline "THink.,convert read unknown things"; (newBoard (), Blank)
	in read charlst 0 (newBoard ())

let convertBackBoard board : string =
	let rec f n : string =
		if (n = 42) 
			then ""
		else match board.(n) with
			| Blank -> "b," ^ f (n+1)
			| P1 -> "x," ^ f (n+1) 
			| P2 -> "o," ^ f (n+1) 
	in f 0 

let convertBack (o:outcome) : string =
	let rec f arr n winstate : string =
		if (n = 42) 
			then winstate
		else match arr.(n) with
			| Blank -> "b," ^ (f arr (n+1) winstate)
			| P1 -> "x," ^ (f arr (n+1) winstate)
			| P2 -> "o," ^ (f arr (n+1) winstate)
	in match o with
		| (arr, Blank) -> f arr 0 "draw"
		| (arr, P1) -> f arr 0 "win"
		| (arr, P2) -> f arr 0 "loss"

let favor (player:spot) =
	match player with
	| P1 -> (>)
	| P2 -> (<)
	| Blank -> print_endline "favor called on Blank"; (=)

let boardComp a b = 
	let rec f x y n =
		if (n = 42) 
			then 0
		else match (x.(n)), (y.(n)) with
			| Blank, Blank -> f x y (n+1)
			| Blank, P1 -> -1
			| Blank, P2 -> -1
			| P1, Blank -> 1
			| P1, P1 -> f x y (n+1)
			| P1, P2 -> -1
			| P2, Blank -> 1
			| P2, P1 -> 1
			| P2, P2 -> f x y (n+1)
	in f a b 0

let sameBoard b1 b2 : bool = 
	(* returns whether they are the same board *)
	let rec f n =
		if (b1.(n) = b2.(n))
			then begin 
					if (n < 41)
						then f (n+1)
					else true 
				end
		else false
	in f 0

let boardSort (arr:outcome array) : outcome array = 
	let f x y = boardComp (fst x) (fst y)
	in Array.sort f arr; arr

let sortData : outcome array = Array.of_list (List.map convert (lines_from_file "sortedData.txt"))
(* a sorted version of the database that Tromp released on the UCI ML Repository *)

let utilSortTromp board : float option = 
(* searches through Tromp's dataset to return the expected outcome of the board *)
(* assumes the data is in a sorted array  *)	
	let rec f min max =
		let mid = (min + max)/2 
		in let x = sortData.(mid) 
		in if (max < min)
				then begin 
					printBoard board; 
					print_endline "board not found in Tromp's dataset."; 
					None
					end
			else begin
				let y = boardComp (fst x) board 
				in if (y = 0)
					then begin 
						match snd x with
						| P1 -> Some 50.
						| Blank -> Some 0.
						| P2 -> Some (-50.)
						end 
				else if (y = -1)
					then f (mid+1) max
				else f min (mid-1)
				end
	in f 0 ((Array.length sortData)-1)

	(* UTILITY LINES FUNCTIONS *)

let willLose board player : int option =
(* checks the 69 limes for 3-groups by the opponent *)
(* returns the index of the spot needed for them to win or None*)
	let rec sm p x =
		if (p = player)
			then x
		else f (n+1)
	and f n = 
		if (n > 69) then None
		else match winLine board n with
			| P1, P1, P1, Blank::[] -> sm P2 n
			| P1, P1, Blank, P1::[] -> sm P2 n
			| P1, Blank, P1, P1::[] -> sm P2 n
			| Blank, P1, P1, P1::[] -> sm P2 n
			(**)
			| P2, P2, P2, Blank::[] -> sm P1 n
			| P2, P2, Blank, P2::[] -> sm P1 n
			| P2, Blank, P2, P2::[] -> sm P1 n
			| Blank, P2, P2, P2::[] -> sm P1 n


let utilLines board : float =
	let eval = ref 10. in
	let rec f lst : unit = 
		match lst with
		| (P1::P1::P1::P1::[])::tl -> eval := 100.;
		| (P2::P2::P2::P2::[])::tl -> eval := -100.;
		| (a::b::c::d::[])::tl ->	
		begin
			match a, b, c, d with
			| P1, P1, P1, Blank -> (eval := !eval +. 40.; f tl;)
			| P1, P1, Blank, P1 -> (eval := !eval +. 40.; f tl;)
			| P1, Blank, P1, P1 -> (eval := !eval +. 40.; f tl;)
			| Blank, P1, P1, P1 -> (eval := !eval +. 40.; f tl;)
			(**)
			| P1, P1, Blank, Blank -> (eval := !eval +. 9.; f tl;)
			| P1, Blank, P1, Blank -> (eval := !eval +. 9.; f tl;)
			| P1, Blank, Blank, P1 -> (eval := !eval +. 9.5; f tl;)
			| Blank, P1, P1, Blank -> (eval := !eval +. 9.; f tl;)
			| Blank, P1, Blank, P1 -> (eval := !eval +. 9.; f tl;)
			| Blank, Blank, P1, P1 -> (eval := !eval +. 9.; f tl;)
			(**)
			| P2, P2, P2, Blank -> (eval := !eval -. 40.; f tl;)
			| P2, P2, Blank, P2 -> (eval := !eval -. 40.; f tl;)
			| P2, Blank, P2, P2 -> (eval := !eval -. 40.; f tl;)
			| Blank, P2, P2, P2 -> (eval := !eval -. 40.; f tl;)
			(**)
			| P2, P2, Blank, Blank -> (eval := !eval -. 9.; f tl;)
			| P2, Blank, P2, Blank -> (eval := !eval -. 9.; f tl;)
			| P2, Blank, Blank, P2 -> (eval := !eval -. 9.5; f tl;)
			| Blank, P2, P2, Blank -> (eval := !eval -. 9.; f tl;)
			| Blank, P2, Blank, P2 -> (eval := !eval -. 9.; f tl;)
			| Blank, Blank, P2, P2 -> (eval := !eval -. 9.; f tl;)
			(**)
			| _ -> f tl;
		end
		| [] -> ()
		| _ -> print_endline "utilLines inset function hit the fail case";
	in f (winLines board); !eval

let utilDots board : float =
	let eval = ref 10. in
	let rec f lst : unit = 
		match lst with
		| (P1::P1::P1::P1::[])::tl -> eval := 100.;
		| (P2::P2::P2::P2::[])::tl -> eval := -100.;
		| (a::b::c::d::[])::tl ->	
		begin
			match a, b, c, d with
			| P1, P1, P1, Blank -> (eval := !eval +. 35.; f tl;)
			| P1, P1, Blank, P1 -> (eval := !eval +. 35.; f tl;)
			| P1, Blank, P1, P1 -> (eval := !eval +. 35.; f tl;)
			| Blank, P1, P1, P1 -> (eval := !eval +. 35.; f tl;)
			(**)
			| P1, P1, Blank, Blank -> (eval := !eval +. 8.; f tl;)
			| P1, Blank, P1, Blank -> (eval := !eval +. 8.; f tl;)
			| P1, Blank, Blank, P1 -> (eval := !eval +. 8.5; f tl;)
			| Blank, P1, P1, Blank -> (eval := !eval +. 8.; f tl;)
			| Blank, P1, Blank, P1 -> (eval := !eval +. 8.; f tl;)
			| Blank, Blank, P1, P1 -> (eval := !eval +. 8.; f tl;)
			(**)
			| P1, Blank, Blank, Blank -> (eval := !eval +. 0.5; f tl;)
			| Blank, P1, Blank, Blank -> (eval := !eval +. 0.5; f tl;)
			| Blank, Blank, P1, Blank -> (eval := !eval +. 0.5; f tl;)
			| Blank, Blank, Blank, P1 -> (eval := !eval +. 0.5; f tl;)
			(**)
			| P2, P2, P2, Blank -> (eval := !eval -. 35.; f tl;)
			| P2, P2, Blank, P2 -> (eval := !eval -. 35.; f tl;)
			| P2, Blank, P2, P2 -> (eval := !eval -. 35.; f tl;)
			| Blank, P2, P2, P2 -> (eval := !eval -. 35.; f tl;)
			(**)
			| P2, P2, Blank, Blank -> (eval := !eval -. 8.; f tl;)
			| P2, Blank, P2, Blank -> (eval := !eval -. 8.; f tl;)
			| P2, Blank, Blank, P2 -> (eval := !eval -. 8.5; f tl;)
			| Blank, P2, P2, Blank -> (eval := !eval -. 8.; f tl;)
			| Blank, P2, Blank, P2 -> (eval := !eval -. 8.; f tl;)
			| Blank, Blank, P2, P2 -> (eval := !eval -. 8.; f tl;)
			(**)
			| P2, Blank, Blank, Blank -> (eval := !eval -. 0.5; f tl;)
			| Blank, P2, Blank, Blank -> (eval := !eval -. 0.5; f tl;)
			| Blank, Blank, P2, Blank -> (eval := !eval -. 0.5; f tl;)
			| Blank, Blank, Blank, P2 -> (eval := !eval -. 0.5; f tl;)
			(**)
			| _ -> f tl;
		end
		| [] -> ()
		| _ -> print_endline "utilLines inset function hit the fail case";
	in f (winLines board); !eval

let lineHeight x : float = 
	if (x < 0) 
		then 0.
	else if (x < 24) (* horizontal *)
		then (9.5 /. (float ((x mod 6) + 7)))
	else if (x < 45) (* vertical *)
		then (11. /. (float (((x-24) mod 3) + 10)))
	else if (x < 57) (* forward slash *)
		then (11. /. (float (((x-45) mod 3) + 10)))
	else if (x < 69) (* back slash *)
		then (11. /. (float (((x-57) mod 3) + 10)))
	else 0.	

let utilHeightLines board : float =
	let eval = ref 10.
	in let rec f n : float = 
		if (n < 69) 
			then begin
			match winLine board n with
			| P1::P1::P1::P1::[] -> 100.
			| P2::P2::P2::P2::[] -> -100.
			| P1::P1::P1::Blank::[] -> (eval := !eval +. ((lineHeight n) *. 35.); f (n+1))
			| P1::P1::Blank::P1::[] -> (eval := !eval +. ((lineHeight n) *. 35.); f (n+1))
			| P1::Blank::P1::P1::[] -> (eval := !eval +. ((lineHeight n) *. 35.); f (n+1))
			| Blank::P1::P1::P1::[] -> (eval := !eval +. ((lineHeight n) *. 35.); f (n+1))
			(**)
			| P1::P1::Blank::Blank::[] -> (eval := !eval +. ((lineHeight n) *. 7.5); f (n+1))
			| P1::Blank::P1::Blank::[] -> (eval := !eval +. ((lineHeight n) *. 7.5); f (n+1))
			| P1::Blank::Blank::P1::[] -> (eval := !eval +. ((lineHeight n) *. 7.5); f (n+1))
			| Blank::P1::P1::Blank::[] -> (eval := !eval +. ((lineHeight n) *. 7.5); f (n+1))
			| Blank::P1::Blank::P1::[] -> (eval := !eval +. ((lineHeight n) *. 7.5); f (n+1))
			| Blank::Blank::P1::P1::[] -> (eval := !eval +. ((lineHeight n) *. 7.5); f (n+1))
			(**)
			| P2::P2::P2::Blank::[] -> (eval := !eval -. ((lineHeight n) *. 35.); f (n+1))
			| P2::P2::Blank::P2::[] -> (eval := !eval -. ((lineHeight n) *. 35.); f (n+1))
			| P2::Blank::P2::P2::[] -> (eval := !eval -. ((lineHeight n) *. 35.); f (n+1))
			| Blank::P2::P2::P2::[] -> (eval := !eval -. ((lineHeight n) *. 35.); f (n+1))
			(**)
			| P2::P2::Blank::Blank::[] -> (eval := !eval -. ((lineHeight n) *. 7.5); f (n+1))
			| P2::Blank::P2::Blank::[] -> (eval := !eval -. ((lineHeight n) *. 7.5); f (n+1))
			| P2::Blank::Blank::P2::[] -> (eval := !eval -. ((lineHeight n) *. 7.5); f (n+1))
			| Blank::P2::P2::Blank::[] -> (eval := !eval -. ((lineHeight n) *. 7.5); f (n+1))
			| Blank::P2::Blank::P2::[] -> (eval := !eval -. ((lineHeight n) *. 7.5); f (n+1))
			| Blank::Blank::P2::P2::[] -> (eval := !eval -. ((lineHeight n) *. 7.5); f (n+1))
			| _ -> f (n+1)
			end
		else !eval
	in f 0

let utilHeightDots board : float =
	let eval = ref 10.
	in let rec f n : float = 
		if (n < 69) 
			then begin
			match winLine board n with
			| P1::P1::P1::P1::[] -> 100.
			| P2::P2::P2::P2::[] -> -100.
			| P1::P1::P1::Blank::[] -> (eval := !eval +. ((lineHeight n) *. 30.); f (n+1))
			| P1::P1::Blank::P1::[] -> (eval := !eval +. ((lineHeight n) *. 30.); f (n+1))
			| P1::Blank::P1::P1::[] -> (eval := !eval +. ((lineHeight n) *. 30.); f (n+1))
			| Blank::P1::P1::P1::[] -> (eval := !eval +. ((lineHeight n) *. 30.); f (n+1))
			(**)
			| P1::P1::Blank::Blank::[] -> (eval := !eval +. ((lineHeight n) *. 7.); f (n+1))
			| P1::Blank::P1::Blank::[] -> (eval := !eval +. ((lineHeight n) *. 7.); f (n+1))
			| P1::Blank::Blank::P1::[] -> (eval := !eval +. ((lineHeight n) *. 7.); f (n+1))
			| Blank::P1::P1::Blank::[] -> (eval := !eval +. ((lineHeight n) *. 7.); f (n+1))
			| Blank::P1::Blank::P1::[] -> (eval := !eval +. ((lineHeight n) *. 7.); f (n+1))
			| Blank::Blank::P1::P1::[] -> (eval := !eval +. ((lineHeight n) *. 7.); f (n+1))
			(**)
			| P1::Blank::Blank::Blank::[] -> (eval := !eval +. ((lineHeight n) *. 0.25); f (n+1))
			| Blank::P1::Blank::Blank::[] -> (eval := !eval +. ((lineHeight n) *. 0.25); f (n+1))
			| Blank::Blank::P1::Blank::[] -> (eval := !eval +. ((lineHeight n) *. 0.25); f (n+1))
			| Blank::Blank::Blank::P1::[] -> (eval := !eval +. ((lineHeight n) *. 0.25); f (n+1))
			(**)
			| P2::P2::P2::Blank::[] -> (eval := !eval -. ((lineHeight n) *. 30.); f (n+1))
			| P2::P2::Blank::P2::[] -> (eval := !eval -. ((lineHeight n) *. 30.); f (n+1))
			| P2::Blank::P2::P2::[] -> (eval := !eval -. ((lineHeight n) *. 30.); f (n+1))
			| Blank::P2::P2::P2::[] -> (eval := !eval -. ((lineHeight n) *. 30.); f (n+1))
			(**)
			| P2::P2::Blank::Blank::[] -> (eval := !eval -. ((lineHeight n) *. 7.); f (n+1))
			| P2::Blank::P2::Blank::[] -> (eval := !eval -. ((lineHeight n) *. 7.); f (n+1))
			| P2::Blank::Blank::P2::[] -> (eval := !eval -. ((lineHeight n) *. 7.); f (n+1))
			| Blank::P2::P2::Blank::[] -> (eval := !eval -. ((lineHeight n) *. 7.); f (n+1))
			| Blank::P2::Blank::P2::[] -> (eval := !eval -. ((lineHeight n) *. 7.); f (n+1))
			| Blank::Blank::P2::P2::[] -> (eval := !eval -. ((lineHeight n) *. 7.); f (n+1))
			(**)
			| P2::Blank::Blank::Blank::[] -> (eval := !eval -. ((lineHeight n) *. 0.25); f (n+1))
			| Blank::P2::Blank::Blank::[] -> (eval := !eval -. ((lineHeight n) *. 0.25); f (n+1))
			| Blank::Blank::P2::Blank::[] -> (eval := !eval -. ((lineHeight n) *. 0.25); f (n+1))
			| Blank::Blank::Blank::P2::[] -> (eval := !eval -. ((lineHeight n) *. 0.25); f (n+1))
			| _ -> f (n+1)
			end
		else !eval
	in f 0

let lineWeight x : float = 
	if (x < 0) 
		then 0.
	else if (x < 6) (* horizontal edge *)
		then 0.9
	else if (x < 18) (* horizontal center *)
		then 1.15
	else if (x < 24) (* horizontal edge *)
		then 0.9
	else if (x < 27) (* vertical *)
		then 0.9
	else if (x < 30) (* vertical *)
		then 0.95
	else if (x < 33) (* vertical *)
		then 1.
	else if (x < 36) (* vertical *)
		then 1.05
	else if (x < 39) (* vertical *)
		then 1.
	else if (x < 42) (* vertical *)
		then 0.95
	else if (x < 45) (* vertical *)
		then 0.9
	else if (x < 48) (* forward slash *)
		then 0.95
	else if (x < 54) (* forward slash *)
		then 1.2
	else if (x < 57) (* forward slash *)
		then 0.95
	else if (x < 60) (* back slash *)
		then 0.95
	else if (x < 66) (* back slash *)
		then 1.2
	else if (x < 69) (* back slash *)
		then 0.95
	else 0.

let utilWeightLines board : float =
	let eval = ref 10.
	in let rec f n : float = 
		if (n < 69) 
			then begin
			match winLine board n with
			| P1::P1::P1::P1::[] -> 100.
			| P2::P2::P2::P2::[] -> -100.
			| P1::P1::P1::Blank::[] -> (eval := !eval +. ((lineWeight n) *. 35.); f (n+1))
			| P1::P1::Blank::P1::[] -> (eval := !eval +. ((lineWeight n) *. 35.); f (n+1))
			| P1::Blank::P1::P1::[] -> (eval := !eval +. ((lineWeight n) *. 35.); f (n+1))
			| Blank::P1::P1::P1::[] -> (eval := !eval +. ((lineWeight n) *. 35.); f (n+1))
			(**)
			| P1::P1::Blank::Blank::[] -> (eval := !eval +. ((lineWeight n) *. 7.5); f (n+1))
			| P1::Blank::P1::Blank::[] -> (eval := !eval +. ((lineWeight n) *. 7.5); f (n+1))
			| P1::Blank::Blank::P1::[] -> (eval := !eval +. ((lineWeight n) *. 7.5); f (n+1))
			| Blank::P1::P1::Blank::[] -> (eval := !eval +. ((lineWeight n) *. 7.5); f (n+1))
			| Blank::P1::Blank::P1::[] -> (eval := !eval +. ((lineWeight n) *. 7.5); f (n+1))
			| Blank::Blank::P1::P1::[] -> (eval := !eval +. ((lineWeight n) *. 7.5); f (n+1))
			(**)
			| P2::P2::P2::Blank::[] -> (eval := !eval -. ((lineWeight n) *. 35.); f (n+1))
			| P2::P2::Blank::P2::[] -> (eval := !eval -. ((lineWeight n) *. 35.); f (n+1))
			| P2::Blank::P2::P2::[] -> (eval := !eval -. ((lineWeight n) *. 35.); f (n+1))
			| Blank::P2::P2::P2::[] -> (eval := !eval -. ((lineWeight n) *. 35.); f (n+1))
			(**)
			| P2::P2::Blank::Blank::[] -> (eval := !eval -. ((lineWeight n) *. 7.5); f (n+1))
			| P2::Blank::P2::Blank::[] -> (eval := !eval -. ((lineWeight n) *. 7.5); f (n+1))
			| P2::Blank::Blank::P2::[] -> (eval := !eval -. ((lineWeight n) *. 7.5); f (n+1))
			| Blank::P2::P2::Blank::[] -> (eval := !eval -. ((lineWeight n) *. 7.5); f (n+1))
			| Blank::P2::Blank::P2::[] -> (eval := !eval -. ((lineWeight n) *. 7.5); f (n+1))
			| Blank::Blank::P2::P2::[] -> (eval := !eval -. ((lineWeight n) *. 7.5); f (n+1))
			| _ -> f (n+1)
			end
		else !eval
	in f 0

let utilWeightDots board : float =
	let eval = ref 10.
	in let rec f n : float = 
		if (n < 69) 
			then begin
			match winLine board n with
			| P1::P1::P1::P1::[] -> 100.
			| P2::P2::P2::P2::[] -> -100.
			| P1::P1::P1::Blank::[] -> (eval := !eval +. ((lineWeight n) *. 30.); f (n+1))
			| P1::P1::Blank::P1::[] -> (eval := !eval +. ((lineWeight n) *. 30.); f (n+1))
			| P1::Blank::P1::P1::[] -> (eval := !eval +. ((lineWeight n) *. 30.); f (n+1))
			| Blank::P1::P1::P1::[] -> (eval := !eval +. ((lineWeight n) *. 30.); f (n+1))
			(**)
			| P1::P1::Blank::Blank::[] -> (eval := !eval +. ((lineWeight n) *. 7.); f (n+1))
			| P1::Blank::P1::Blank::[] -> (eval := !eval +. ((lineWeight n) *. 7.); f (n+1))
			| P1::Blank::Blank::P1::[] -> (eval := !eval +. ((lineWeight n) *. 7.); f (n+1))
			| Blank::P1::P1::Blank::[] -> (eval := !eval +. ((lineWeight n) *. 7.); f (n+1))
			| Blank::P1::Blank::P1::[] -> (eval := !eval +. ((lineWeight n) *. 7.); f (n+1))
			| Blank::Blank::P1::P1::[] -> (eval := !eval +. ((lineWeight n) *. 7.); f (n+1))
			(**)
			| P1::Blank::Blank::Blank::[] -> (eval := !eval +. ((lineWeight n) *. 0.25); f (n+1))
			| Blank::P1::Blank::Blank::[] -> (eval := !eval +. ((lineWeight n) *. 0.25); f (n+1))
			| Blank::Blank::P1::Blank::[] -> (eval := !eval +. ((lineWeight n) *. 0.25); f (n+1))
			| Blank::Blank::Blank::P1::[] -> (eval := !eval +. ((lineWeight n) *. 0.25); f (n+1))
			(**)
			| P2::P2::P2::Blank::[] -> (eval := !eval -. ((lineWeight n) *. 30.); f (n+1))
			| P2::P2::Blank::P2::[] -> (eval := !eval -. ((lineWeight n) *. 30.); f (n+1))
			| P2::Blank::P2::P2::[] -> (eval := !eval -. ((lineWeight n) *. 30.); f (n+1))
			| Blank::P2::P2::P2::[] -> (eval := !eval -. ((lineWeight n) *. 30.); f (n+1))
			(**)
			| P2::P2::Blank::Blank::[] -> (eval := !eval -. ((lineWeight n) *. 7.); f (n+1))
			| P2::Blank::P2::Blank::[] -> (eval := !eval -. ((lineWeight n) *. 7.); f (n+1))
			| P2::Blank::Blank::P2::[] -> (eval := !eval -. ((lineWeight n) *. 7.); f (n+1))
			| Blank::P2::P2::Blank::[] -> (eval := !eval -. ((lineWeight n) *. 7.); f (n+1))
			| Blank::P2::Blank::P2::[] -> (eval := !eval -. ((lineWeight n) *. 7.); f (n+1))
			| Blank::Blank::P2::P2::[] -> (eval := !eval -. ((lineWeight n) *. 7.); f (n+1))
			(**)
			| P2::Blank::Blank::Blank::[] -> (eval := !eval -. ((lineWeight n) *. 0.25); f (n+1))
			| Blank::P2::Blank::Blank::[] -> (eval := !eval -. ((lineWeight n) *. 0.25); f (n+1))
			| Blank::Blank::P2::Blank::[] -> (eval := !eval -. ((lineWeight n) *. 0.25); f (n+1))
			| Blank::Blank::Blank::P2::[] -> (eval := !eval -. ((lineWeight n) *. 0.25); f (n+1))
			| _ -> f (n+1)
			end
		else !eval
	in f 0

let utilHWLines board : float =
	let eval = ref 10.
	in let rec f n : float = 
		if (n < 69) 
			then begin
			match winLine board n with
			| P1::P1::P1::P1::[] -> 100.
			| P2::P2::P2::P2::[] -> -100.
			| P1::P1::P1::Blank::[] -> (eval := !eval +. ((lineHeight n) *. (lineWeight n) *. 35.); f (n+1))
			| P1::P1::Blank::P1::[] -> (eval := !eval +. ((lineHeight n) *. (lineWeight n) *. 35.); f (n+1))
			| P1::Blank::P1::P1::[] -> (eval := !eval +. ((lineHeight n) *. (lineWeight n) *. 35.); f (n+1))
			| Blank::P1::P1::P1::[] -> (eval := !eval +. ((lineHeight n) *. (lineWeight n) *. 35.); f (n+1))
			(**)
			| P1::P1::Blank::Blank::[] -> (eval := !eval +. ((lineHeight n) *. (lineWeight n) *. 7.5); f (n+1))
			| P1::Blank::P1::Blank::[] -> (eval := !eval +. ((lineHeight n) *. (lineWeight n) *. 7.5); f (n+1))
			| P1::Blank::Blank::P1::[] -> (eval := !eval +. ((lineHeight n) *. (lineWeight n) *. 7.5); f (n+1))
			| Blank::P1::P1::Blank::[] -> (eval := !eval +. ((lineHeight n) *. (lineWeight n) *. 7.5); f (n+1))
			| Blank::P1::Blank::P1::[] -> (eval := !eval +. ((lineHeight n) *. (lineWeight n) *. 7.5); f (n+1))
			| Blank::Blank::P1::P1::[] -> (eval := !eval +. ((lineHeight n) *. (lineWeight n) *. 7.5); f (n+1))
			(**)
			| P2::P2::P2::Blank::[] -> (eval := !eval -. ((lineHeight n) *. (lineWeight n) *. 35.); f (n+1))
			| P2::P2::Blank::P2::[] -> (eval := !eval -. ((lineHeight n) *. (lineWeight n) *. 35.); f (n+1))
			| P2::Blank::P2::P2::[] -> (eval := !eval -. ((lineHeight n) *. (lineWeight n) *. 35.); f (n+1))
			| Blank::P2::P2::P2::[] -> (eval := !eval -. ((lineHeight n) *. (lineWeight n) *. 35.); f (n+1))
			(**)
			| P2::P2::Blank::Blank::[] -> (eval := !eval -. ((lineHeight n) *. (lineWeight n) *. 7.5); f (n+1))
			| P2::Blank::P2::Blank::[] -> (eval := !eval -. ((lineHeight n) *. (lineWeight n) *. 7.5); f (n+1))
			| P2::Blank::Blank::P2::[] -> (eval := !eval -. ((lineHeight n) *. (lineWeight n) *. 7.5); f (n+1))
			| Blank::P2::P2::Blank::[] -> (eval := !eval -. ((lineHeight n) *. (lineWeight n) *. 7.5); f (n+1))
			| Blank::P2::Blank::P2::[] -> (eval := !eval -. ((lineHeight n) *. (lineWeight n) *. 7.5); f (n+1))
			| Blank::Blank::P2::P2::[] -> (eval := !eval -. ((lineHeight n) *. (lineWeight n) *. 7.5); f (n+1))
			| _ -> f (n+1)
			end
		else !eval
	in f 0

let utilHWDots board : float =
	let eval = ref 10.
	in let rec f n : float = 
		if (n < 69) 
			then begin
			match winLine board n with
			| P1::P1::P1::P1::[] -> 100.
			| P2::P2::P2::P2::[] -> -100.
			| P1::P1::P1::Blank::[] -> (eval := !eval +. ((lineHeight n) *. (lineWeight n) *. 30.); f (n+1))
			| P1::P1::Blank::P1::[] -> (eval := !eval +. ((lineHeight n) *. (lineWeight n) *. 30.); f (n+1))
			| P1::Blank::P1::P1::[] -> (eval := !eval +. ((lineHeight n) *. (lineWeight n) *. 30.); f (n+1))
			| Blank::P1::P1::P1::[] -> (eval := !eval +. ((lineHeight n) *. (lineWeight n) *. 30.); f (n+1))
			(**)
			| P1::P1::Blank::Blank::[] -> (eval := !eval +. ((lineHeight n) *. (lineWeight n) *. 7.); f (n+1))
			| P1::Blank::P1::Blank::[] -> (eval := !eval +. ((lineHeight n) *. (lineWeight n) *. 7.); f (n+1))
			| P1::Blank::Blank::P1::[] -> (eval := !eval +. ((lineHeight n) *. (lineWeight n) *. 7.); f (n+1))
			| Blank::P1::P1::Blank::[] -> (eval := !eval +. ((lineHeight n) *. (lineWeight n) *. 7.); f (n+1))
			| Blank::P1::Blank::P1::[] -> (eval := !eval +. ((lineHeight n) *. (lineWeight n) *. 7.); f (n+1))
			| Blank::Blank::P1::P1::[] -> (eval := !eval +. ((lineHeight n) *. (lineWeight n) *. 7.); f (n+1))
			(**)
			| P1::Blank::Blank::Blank::[] -> (eval := !eval +. ((lineHeight n) *. (lineWeight n) *. 0.25); f (n+1))
			| Blank::P1::Blank::Blank::[] -> (eval := !eval +. ((lineHeight n) *. (lineWeight n) *. 0.25); f (n+1))
			| Blank::Blank::P1::Blank::[] -> (eval := !eval +. ((lineHeight n) *. (lineWeight n) *. 0.25); f (n+1))
			| Blank::Blank::Blank::P1::[] -> (eval := !eval +. ((lineHeight n) *. (lineWeight n) *. 0.25); f (n+1))
			(**)
			| P2::P2::P2::Blank::[] -> (eval := !eval -. ((lineHeight n) *. (lineWeight n) *. 30.); f (n+1))
			| P2::P2::Blank::P2::[] -> (eval := !eval -. ((lineHeight n) *. (lineWeight n) *. 30.); f (n+1))
			| P2::Blank::P2::P2::[] -> (eval := !eval -. ((lineHeight n) *. (lineWeight n) *. 30.); f (n+1))
			| Blank::P2::P2::P2::[] -> (eval := !eval -. ((lineHeight n) *. (lineWeight n) *. 30.); f (n+1))
			(**)
			| P2::P2::Blank::Blank::[] -> (eval := !eval -. ((lineHeight n) *. (lineWeight n) *. 7.); f (n+1))
			| P2::Blank::P2::Blank::[] -> (eval := !eval -. ((lineHeight n) *. (lineWeight n) *. 7.); f (n+1))
			| P2::Blank::Blank::P2::[] -> (eval := !eval -. ((lineHeight n) *. (lineWeight n) *. 7.); f (n+1))
			| Blank::P2::P2::Blank::[] -> (eval := !eval -. ((lineHeight n) *. (lineWeight n) *. 7.); f (n+1))
			| Blank::P2::Blank::P2::[] -> (eval := !eval -. ((lineHeight n) *. (lineWeight n) *. 7.); f (n+1))
			| Blank::Blank::P2::P2::[] -> (eval := !eval -. ((lineHeight n) *. (lineWeight n) *. 7.); f (n+1))
			(**)
			| P2::Blank::Blank::Blank::[] -> (eval := !eval -. ((lineHeight n) *. (lineWeight n) *. 0.25); f (n+1))
			| Blank::P2::Blank::Blank::[] -> (eval := !eval -. ((lineHeight n) *. (lineWeight n) *. 0.25); f (n+1))
			| Blank::Blank::P2::Blank::[] -> (eval := !eval -. ((lineHeight n) *. (lineWeight n) *. 0.25); f (n+1))
			| Blank::Blank::Blank::P2::[] -> (eval := !eval -. ((lineHeight n) *. (lineWeight n) *. 0.25); f (n+1))
			| _ -> f (n+1)
			end
		else !eval
	in f 0

let utilCowardLines board : float =
	let eval1 = ref 0. in
	let eval2 = ref 0. in
	let rec f lst : float = 
		match lst with
		| (P1::P1::P1::P1::[])::tl -> 100.
		| (P2::P2::P2::P2::[])::tl -> -100.
		| (a::b::c::d::[])::tl ->	
		begin
			match a, b, c, d with
			| P1, P1, P1, Blank -> (eval1 := !eval1 +. 40.; f tl;)
			| P1, P1, Blank, P1 -> (eval1 := !eval1 +. 40.; f tl;)
			| P1, Blank, P1, P1 -> (eval1 := !eval1 +. 40.; f tl;)
			| Blank, P1, P1, P1 -> (eval1 := !eval1 +. 40.; f tl;)
			(**)
			| P1, P1, Blank, Blank -> (eval1 := !eval1 +. 9.; f tl;)
			| P1, Blank, P1, Blank -> (eval1 := !eval1 +. 9.; f tl;)
			| P1, Blank, Blank, P1 -> (eval1 := !eval1 +. 9.5; f tl;)
			| Blank, P1, P1, Blank -> (eval1 := !eval1 +. 9.; f tl;)
			| Blank, P1, Blank, P1 -> (eval1 := !eval1 +. 9.; f tl;)
			| Blank, Blank, P1, P1 -> (eval1 := !eval1 +. 9.; f tl;)
			(**)
			| P2, P2, P2, Blank -> (eval2 := !eval2 +. 40.; f tl;)
			| P2, P2, Blank, P2 -> (eval2 := !eval2 +. 40.; f tl;)
			| P2, Blank, P2, P2 -> (eval2 := !eval2 +. 40.; f tl;)
			| Blank, P2, P2, P2 -> (eval2 := !eval2 +. 40.; f tl;)
			(**)
			| P2, P2, Blank, Blank -> (eval2 := !eval2 +. 9.; f tl;)
			| P2, Blank, P2, Blank -> (eval2 := !eval2 +. 9.; f tl;)
			| P2, Blank, Blank, P2 -> (eval2 := !eval2 +. 9.5; f tl;)
			| Blank, P2, P2, Blank -> (eval2 := !eval2 +. 9.; f tl;)
			| Blank, P2, Blank, P2 -> (eval2 := !eval2 +. 9.; f tl;)
			| Blank, Blank, P2, P2 -> (eval2 := !eval2 +. 9.; f tl;)
			(**)
			| _ -> f tl;
		end
		| [] -> begin
			match curPlayer (whatPly board) with
			| P1 -> !eval1 -. (!eval2 *. 1.5)
			| P2 -> (!eval1 *. 1.5) -. !eval2
			| Blank -> print_endline "utilCowardLines shouldn't have reached this"; 0.
				end
		| _ -> print_endline "utilLines inset function hit the fail case"; 0.
	in f (winLines board)

let utilCowardDots board : float =
	let eval1 = ref 0. in
	let eval2 = ref 0. in
	let rec f lst : float = 
		match lst with
		| (P1::P1::P1::P1::[])::tl -> 100.
		| (P2::P2::P2::P2::[])::tl -> -100.
		| (a::b::c::d::[])::tl ->	
		begin
			match a, b, c, d with
			| P1, P1, P1, Blank -> (eval1 := !eval1 +. 35.; f tl;)
			| P1, P1, Blank, P1 -> (eval1 := !eval1 +. 35.; f tl;)
			| P1, Blank, P1, P1 -> (eval1 := !eval1 +. 35.; f tl;)
			| Blank, P1, P1, P1 -> (eval1 := !eval1 +. 35.; f tl;)
			(**)
			| P1, P1, Blank, Blank -> (eval1 := !eval1 +. 8.; f tl;)
			| P1, Blank, P1, Blank -> (eval1 := !eval1 +. 8.; f tl;)
			| P1, Blank, Blank, P1 -> (eval1 := !eval1 +. 8.5; f tl;)
			| Blank, P1, P1, Blank -> (eval1 := !eval1 +. 8.; f tl;)
			| Blank, P1, Blank, P1 -> (eval1 := !eval1 +. 8.; f tl;)
			| Blank, Blank, P1, P1 -> (eval1 := !eval1 +. 8.; f tl;)
			(**)
			| P1, Blank, Blank, Blank -> (eval1 := !eval1 +. 0.5; f tl;)
			| Blank, P1, Blank, Blank -> (eval1 := !eval1 +. 0.5; f tl;)
			| Blank, Blank, P1, Blank -> (eval1 := !eval1 +. 0.5; f tl;)
			| Blank, Blank, Blank, P1 -> (eval1 := !eval1 +. 0.5; f tl;)
			(**)
			| P2, P2, P2, Blank -> (eval2 := !eval2 +. 35.; f tl;)
			| P2, P2, Blank, P2 -> (eval2 := !eval2 +. 35.; f tl;)
			| P2, Blank, P2, P2 -> (eval2 := !eval2 +. 35.; f tl;)
			| Blank, P2, P2, P2 -> (eval2 := !eval2 +. 35.; f tl;)
			(**)
			| P2, P2, Blank, Blank -> (eval2 := !eval2 +. 8.; f tl;)
			| P2, Blank, P2, Blank -> (eval2 := !eval2 +. 8.; f tl;)
			| P2, Blank, Blank, P2 -> (eval2 := !eval2 +. 8.5; f tl;)
			| Blank, P2, P2, Blank -> (eval2 := !eval2 +. 8.; f tl;)
			| Blank, P2, Blank, P2 -> (eval2 := !eval2 +. 8.; f tl;)
			| Blank, Blank, P2, P2 -> (eval2 := !eval2 +. 8.; f tl;)
			(**)
			| P2, Blank, Blank, Blank -> (eval2 := !eval2 +. 0.5; f tl;)
			| Blank, P2, Blank, Blank -> (eval2 := !eval2 +. 0.5; f tl;)
			| Blank, Blank, P2, Blank -> (eval2 := !eval2 +. 0.5; f tl;)
			| Blank, Blank, Blank, P2 -> (eval2 := !eval2 +. 0.5; f tl;)
			(**)
			| _ -> f tl;
		end
		| [] -> begin
			match curPlayer (whatPly board) with
			| P1 -> !eval1 -. (!eval2 *. 1.5)
			| P2 -> (!eval1 *. 1.5) -. !eval2
			| Blank -> print_endline "utilCowardLines shouldn't have reached this"; 0.
				end
		| _ -> print_endline "utilLines inset function hit the fail case"; 0.
	in f (winLines board)
	

(* STATE SPACE SEARCH-BASED UTILITY FUNCTIONS *)

let utilAvg board n util : float = 
	(* let heurRuns = ref 0 in *) 
	let player = curPlayer (whatPly board)
	in let rec f n b =
		match gameOver b with
		| Some P1 -> 100.
		| Some P2 -> -100.
		| Some Blank -> 0.
		| None ->
			if (n < 1)
				then util b	(* heurRuns := !heurRuns + 1; *)
			else let pm = posMoves player b
				in (List.fold_left (+.) 0. (List.map (f (n-1)) pm)) /. (float (List.length pm))
	in f n board

(* MASSIVELY BORKED *)
(* THIS RUNS INTO LEGAL MOVES THAT AREN'T IN THE TROMP DATASET *)
let rec utilAvgTromp (board:board) : float =
	let ply = whatPly board
	in let player = curPlayer ply
	in if (ply < 8) 
		then let pm = posMoves player board
			in ((List.fold_left (+.) 0. (List.map utilAvgTromp pm)) /. (float (List.length pm)))
	else if (ply > 8)
		then utilAvg board (41-ply) utilLines
	else match utilSortTromp board with
		| Some x -> x
		| None -> print_endline "Tromp done fucked up..."; 5.