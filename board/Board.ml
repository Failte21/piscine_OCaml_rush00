let (--) i j =
	let rec aux n acc =
	  if n < i then acc else aux (n-1) (n :: acc) in
	aux j []

let ( ** ) a b =
	int_of_float (float_of_int a ** float_of_int b)

type t =
	| Board of t list
	| Conquered of Player.t
	| Free

type move = int list

let is_full boards =
	List.for_all (fun e -> match e with
		| Conquered _ -> true
		| _ -> false
	) boards

let rec check_h l p =
  match l with 
	| a::b::c::t -> (match a with
		| Conquered p2 when p2 = p -> (match b with
			| Conquered p2 when p2 = p -> (match c with
				| Conquered p2 when p2 = p -> true
				| _ -> false
			)
			| _ -> false
		)
		| _ -> false || check_h t p
	)
	| _ -> false

let rec check_v l p =
  match l with 
	| [a; b; c; d; e; f; g; h; i] -> (match a with
		| Conquered p2 when p2 = p -> (match d with
			| Conquered p2 when p2 = p -> (match g with
				| Conquered p2 when p2 = p -> true
				| _ -> false
			)
			| _ -> false
		)
		| _ -> false
	) || (match b with
    | Conquered p2 when p2 = p -> (match e with
      | Conquered p2 when p2 = p -> (match h with
        | Conquered p2 when p2 = p -> true
        | _ -> false
      )
      | _ -> false
    )
    | _ -> false
  ) || (match c with
    | Conquered p2 when p2 = p -> (match f with
      | Conquered p2 when p2 = p -> (match i with
        | Conquered p2 when p2 = p -> true
        | _ -> false
      )
      | _ -> false
    )
    | _ -> false
  )
	| _ -> false

let rec check_d_a l p =
  match l with 
	| _::_::a::_::b::_::c::_ -> (match a with
		| Conquered p2 when p2 = p -> (match b with
			| Conquered p2 when p2 = p -> (match c with
				| Conquered p2 when p2 = p -> true
				| _ -> false
			)
			| _ -> false
		)
		| _ -> false
	)
	| _ -> false

let rec check_d_b l p =
  match l with 
	| a::_::_::_::b::_::_::_::c::[] -> (match a with
		| Conquered p2 when p2 = p -> (match b with
			| Conquered p2 when p2 = p -> (match c with
				| Conquered p2 when p2 = p -> true
				| _ -> false
			)
			| _ -> false
		)
		| _ -> false
	)
	| _ -> false

let three_row board player =
  check_v board player ||
  check_h board player ||
  check_d_a board player ||
  check_d_b board player

let check_win board player = is_full board || three_row board player

let rec update_b board n player =
  match board with
    | h::t when n = 0 -> (Conquered player)::t
    | h::t -> h::(update_b t (n - 1) player)
    | e -> Printf.printf "..."; e 

let rec update_b_if board n player acc =
  match board with
    | h::t when n = 0 -> (
      match h with
        | Conquered _ -> []
        | _ -> [Conquered player]
    )
    | h::t -> update_b_if t (n - 1) player acc
    | e -> acc

  
let rec minmax_aux best cpt b board this_player opponent player_int =
  let player = if player_int = 1 then this_player else opponent in
  print_endline "...";
  match b with
    | h::t -> (
      match h with
        | Conquered _ -> minmax_aux best (cpt + 1) t board this_player opponent player_int
        | _ -> (
          let updated_board = update_b board cpt player in
          let score = -(minmax updated_board this_player opponent (-player_int)) in
          Printf.printf "score: %d" score;
          let new_best = max score best in
          minmax_aux new_best (cpt + 1) t updated_board this_player opponent player_int
        )
    )
    | [] -> if best == -2 then 0 else best

and minmax board this_player opponent player_int =
  let pwin = check_win board this_player in
  let owin = check_win board opponent in
  if pwin then if player_int = 1 then 1 else (-1) else
  if owin then if player_int = 1 then (-1) else 1 else
  minmax_aux (-2) 0 board board this_player opponent player_int

(* private func minimax(board: Array<Player>, player: Player) -> Int {
    if let winner = self.getWinner(board) as Player {
      return winner.rawValue * player.rawValue // -1 * -1 || 1 * 1
    }

    var move = -1
    var score = -2

    for var i = 0; i < 9; ++i { // For all moves
        if board[i] == Player.Blank { // Only possible moves
            var boardWithNewMove = board // Copy board to make it mutable
            boardWithNewMove[i] = player // Try the move
            let scoreForTheMove = -self.minimax(boardWithNewMove, player: self.getOponnentFor(player)) // Count negative score for oponnent
            if scoreForTheMove > score {
                score = scoreForTheMove
                move = i
            } // Picking move that gives oponnent the worst score
        }
    }
    if move == -1 {
        return 0 // No move - it's a draw
    }
    return score
} *)

let available_boards board player =
  List.fold_left (fun acc i -> (
    acc @ (update_b_if board i player [])
  )) [] (0--9)

let findBestMove board this_player opponent =
  let available_moves = available_boards board this_player in
  let start = 9 - (List.length available_moves) in
  let rec findBestMove_aux current_board best_score best_move i =
    match current_board with
      | h::t -> (
        let score = minmax current_board this_player opponent 1 in
        let (new_best_move, new_best_score) = if score > best_score then (i, score) else (best_move, best_score) in
        Printf.printf "score %d, i: %d\n" new_best_score new_best_move;
        findBestMove_aux t new_best_score new_best_move (i + 1)
      )
      | _ -> best_move in 
    findBestMove_aux available_moves (-1) 0 start

let rec update_case boards player move moves is_last =
	let rec update_case_aux board n = match board with
	    | h::t when (move - 1) = n -> (
        if is_last then (
          match h with
            | Free -> ((Conquered player)::t, true)
            | _ -> ([], false)
        ) else
          let (res, success) = _play h player moves in
          (res::t, success)
	    )
      | h::t ->
        let (res, success) = update_case_aux t (n + 1) in
        (h::res, success)
	    | _ -> ([], false) in
	update_case_aux boards 0

and _play board player moves =
	match moves with
		| h::[] -> ( match board with
      | Board boards -> 
        let (new_boards, success) = update_case boards player h [] true in
        (Board new_boards, success)
			| _ -> (Board [], false)
		)
		| h::t -> ( match board with
			| Board boards ->
        let (new_boards, success) = update_case boards player h t false in
        (Board new_boards, success)
			| _ -> (Board [], false)
		)
		| _ -> (Board [], false)

let rec getCase board move: t =
	match board with
		| Board boards -> ( match move with
				| h::t -> getCase (List.nth boards (h - 1)) t
				| [] -> Board boards
		)
		| e -> e

let update board player = if check_win board player then Conquered player else Board board

let rec updateAll board player =
	match board with
		| Board boards -> update (List.map (fun e -> updateAll e player) boards) player
		| e -> e

let play board player moves =
  let (board_after_play, success) = _play board player moves in
  if success then
  	let updated_board = (updateAll board_after_play player) in
    Ok updated_board
  else Error "Illegal move"

let checkWin = function
	| Conquered player -> Some player
	| _ -> None

let newBoard = function
	| 0 -> Free
	| i when i < 0 -> Free (* TODO: Error *)
	| depth ->
		let rec newBoard_aux depth =
			match depth with
				| i when i > 0 -> Board ((List.map (fun _ -> newBoard_aux (i - 1))) (0--8))
				| _ -> Free in
			newBoard_aux depth

let getLineSizeChar order = (8 * 3 ** (order - 1))
let getNumberLinesChar order = (4 * (3 ** (order - 1)))

let getLineSizePiece order = 3 ** order

let getMove (x, y) order =
	let x = x / getLineSizePiece (order - 1) in
	let y = y / getLineSizePiece (order - 1) in
	x + (y * 3)

let rec getCase board move: t =
	match board with
		| Board boards -> (
			match move with
				| h::t -> getCase (List.nth boards h) t
				| [] -> Board boards
		)
		| e -> e

let subOrder (x, y) order = (x mod (getLineSizePiece (order - 1)), y mod (getLineSizePiece (order - 1)))

let getMovesOfPixel (x, y) order =
	let rec loop acc (x, y) order = match order with
		| order when order <= 0 -> acc
		| order -> loop (acc @ [getMove (x, y) order]) (subOrder (x, y) order) (order - 1)
	in loop [] (x, y) order

let oneCase (x, y) board order =
	let move = getMovesOfPixel (x, y) order in
	match getCase board move with
		| Conquered player -> " " ^ Player.toString player
		| _ -> " -"

let withBorder (x, y) board order =
	oneCase (x, y) board order ^ " |"

let getRepeat order orderbase = 3 ** (orderbase - order)

let ySepLine y order =
	let rec repeat s n = match n with
		| 0 -> ""
		| y -> s ^ repeat s (n - 1)
	in
	let rec loop y order orderbase = match y with
		| y when (y + 1) mod (3 ** (order - 1)) = 0 ->
			"\n " ^ repeat (String.make (getLineSizeChar order - 3) '-' ^ " | ") (getRepeat order orderbase) ^ "\n"
		| y -> loop y (order - 1) orderbase
	in loop y order order

let toString board order =
	let max = getLineSizePiece order in
	let rec loop acc (x, y) max = match (x, y) with
		| (x, y) when x >= max && y >= max - 1 ->
			acc ^ "\n"
		| (x, y) when (y + 1) mod 3 = 0 && x >= max ->
			loop (acc ^ ySepLine y order) (0, y + 1) max
		| (x, y) when x >= max ->
			loop (acc ^ "\n") (0, y + 1) max
		| (x, y) when (x + 1) mod 3 = 0 ->
			loop (acc ^ withBorder (x, y) board order) (x + 1, y) max
		| (x, y) ->
			loop (acc ^ oneCase (x, y) board order) (x + 1, y) max
	in loop "" (0, 0) max
