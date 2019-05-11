(* An OCaml implementation of The Captain's Mistress (aka Connect Four) *)
(* Includes various agents for playing the game *)
(* Rules and game processing functions are in game.ml *)
(* Data processing and utility functions are in util.ml *)
(* Search functions and agents are in search.ml *)
(* The play module and functors are in play.ml *)
(* test.ml contains testing functions and should be compiled first *)
(* By Cassandra Bleskachek *)
(* to use, please run '#use "test.ml";;' *)

type spot = (* each board position can be blank, x, or o *)
| Blank (*-*)
| P1 (*x*)
| P2 (*o*)

type board = spot array
type outcome = (board * spot) (* board and the winner *)
type line = spot list

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

let opponent player : spot =
	match player with
	| P1 -> P2
	| P2 -> P1
	| Blank -> Blank

let curPlayer ply = 
	if ((ply mod 2) = 0)
		then P1
	else P2

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
		| _ -> if (n = 41) then true else f (n+6)
	in (f 5)

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

let drop (player:spot) (column:int) (board:board) : board option =
(* returns the board produced by player dropping a piece into column, or None *)
(* test a move with debugging active *)
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
								(* thisGame := x::!thisGame; *)
								Some x
							   end
					| _ -> f (n+1)
			end;
	in f (column * 6)
	
let whatPly (b:board) : int =
	let rec f n sum =
		if (n > 41)
			then sum
		else begin
			match b.(n) with
			| Blank -> f (n+1) sum
			| _ -> f (n+1) (sum+1)
			end
	in f 0 0

let posMoves (player:spot) (board:board) : board list =
(* returns a list of possible boards the player can move to *)
	match player with
		| Blank -> print_endline "Blank cannot move!"; []
		| _ -> let rec f n = 
				begin
					if n > 6 
						then []
					else match drip player n board with
						| Some b -> b::(f (n+1))
						| None -> f (n+1)
				end;
		in f 0

let posMovesP (board:board) : board list =
(* returns a list of possible boards the player can move to *)
	let player = curPlayer (whatPly board)
	in match player with
		| Blank -> print_endline "Blank cannot move!"; []
		| _ -> let rec f n = 
				begin
					if n > 6 
						then []
					else match drip player n board with
						| Some b -> b::(f (n+1))
						| None -> f (n+1)
				end;
		in f 0

let intWinLines () : unit =
(* prints the 69 lines checked by the utilLines family of functions *)
	print_endline " 5 | 5|11|17|23|29|35|41|";
	print_endline " 4 | 4|10|16|22|28|34|40|";
	print_endline " 3 | 3| 9|15|21|27|33|39|";
	print_endline " 2 | 2| 8|14|20|26|32|38|";
	print_endline " 1 | 1| 7|13|19|25|31|37|";
	print_endline " 0 | 0| 6|12|18|24|30|36|";
	print_endline "    ---------------------";
	print_endline "     0  1  2  3  4  5  6";
	let rec f n =  
		if (n < 0) 
			then begin print_endline "winLines called with n<0"; f(n+1) end
		else if (n < 24) (* horizontal *)
			then begin 
				print_endline ("b.(" 
				^ (string_of_int n) ^ ")::b.(" 
				^ (string_of_int (n+6)) ^ ")::b.("
				^ (string_of_int (n+12)) ^ ")::b.(" 
				^ (string_of_int (n+18)) ^ ")::[]"); 
				f(n+1) 
			end
		else if (n < 45) (* vertical *)
			then begin 
				let i = n-24
				in let x = (i mod 3) + (6 * (i/3))
				in print_endline ("b.(" 
					^ (string_of_int x) ^ ")::b.(" 
					^ (string_of_int (x+1)) ^ ")::b.("
					^ (string_of_int (x+2)) ^ ")::b.(" 
					^ (string_of_int (x+3)) ^ ")::[]"); 
					f(n+1) 
			end
		else if (n < 57) (* forward slash *)
			then begin 
				let i = n-45
				in let x = (i mod 3) + (6 * (i/3))
				in print_endline ("b.(" 
					^ (string_of_int x) ^ ")::b.(" 
					^ (string_of_int (x+7)) ^ ")::b.("
					^ (string_of_int (x+14)) ^ ")::b.(" 
					^ (string_of_int (x+21)) ^ ")::[]");
					f(n+1) 
			end
		else if (n < 69) (* back slash *)
			then begin
				let i = n-57
				in let x = (i mod 3) + (6 * (i/3)) + 18
				in print_endline ("b.(" 
					^ (string_of_int x) ^ ")::b.(" 
					^ (string_of_int (x - 5)) ^ ")::b.("
					^ (string_of_int (x - 10)) ^ ")::b.(" 
					^ (string_of_int (x - 15)) ^ ")::[]");
					f(n+1) 
			end
		else print_endline "winLines called with n>68";
	in f 0

let winLine (b:board) (n:int) : line =
		if (n < 0) 
			then begin print_endline "winLine called with n<0"; [] end
		else if (n < 24) (* horizontal *)
			then b.(n)::b.(n+6)::b.(n+12)::b.(n+18)::[]
		else if (n < 45) (* vertical *)
			then let i = n-24
			in let x = (i mod 3) + (6 * (i/3))
			in b.(x)::b.(x+1)::b.(x+2)::b.(x+3)::[]
		else if (n < 57) (* forward slash *)
			then let i = n-45
			in let x = (i mod 3) + (6 * (i/3))
			in b.(x)::b.(x+7)::b.(x+14)::b.(x+21)::[]
		else if (n < 69) (* back slash *)
			then let i = n-57 (* (n-57)+18 because you only search the right half *)
			in let x = (i mod 3) + (6 * (i/3)) + 18
			in b.(x)::b.(x - 5)::b.(x - 10)::b.(x - 15)::[]
		else begin print_endline "winLine called with n>68"; [] end

	let winLines (b:board): line list = 
		let rec f n = 
			if (n < 69)
				then (winLine b n)::(f (n+1))
			else []
		in f 0

	let hasWon (b:board) (p:spot) : bool =
		if (p = Blank) then (isFull b)  (* check if a draw *)
		else let rec f lst =
			match lst with
			| (w::x::y::z::[])::tl -> 
				if ((p=w) && (p=x) && (p=y) && (p=z)) (* if p has won *)
					then true
				else f tl
			| _::tl -> f tl
			| [] -> false
		in f (winLines b)

	let gameOver (b:board) : spot option =
	(* returns true and the winner if the game is over, false otherwise *)
		if (isFull b) then Some Blank
		else let rec f lst =
				match lst with
				| (P1::P1::P1::P1::[])::tl -> Some P1
				| (P2::P2::P2::P2::[])::tl -> Some P2
				| (_::_::_::_::[])::tl -> f tl
				| [] -> None
				| _ -> print_endline "gameOver's inset function hit the fail case"; None
			in f (winLines b)

	let randomSelect (lst:'a list) : 'a option =
		let rec extract x n = 
			match x with
			| hd::tl -> if (n>0)
							then extract tl (n-1)
						else if(n=0)
							then Some hd
						else (print_endline "randomSelect encountered n<0"; None)
			| [] -> (print_endline "randomSelect encountered empty list"; None)
		in  Random.self_init ();
			extract lst (Random.int (List.length lst))
			