let (--) i j =
	let rec aux n acc =
	  if n < i then acc else aux (n-1) (n :: acc) in
	aux j []

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

let rec check_h = function
	| a::b::c::t -> (match a with
		| Conquered _ -> (match b with
			| Conquered _ -> (match c with
				| Conquered _ -> true
				| _ -> false
			)
			| _ -> false
		)
		| _ -> false
	)
	| _ -> false

let rec check_v = function
	| a::t::e::b::f::g::c::h -> (match a with
		| Conquered _ -> (match b with
			| Conquered _ -> (match c with
				| Conquered _ -> true
				| _ -> false
			)
			| _ -> false
		)
		| _ -> false
	)
	| _ -> false

let rec check_d_a = function
	| _::_::a::_::b::_::c::_ -> (match a with
		| Conquered _ -> (match b with
			| Conquered _ -> (match c with
				| Conquered _ -> true
				| _ -> false
			)
			| _ -> false
		)
		| _ -> false
	)
	| _ -> false

let rec check_d_b = function
	| a::_::_::_::b::_::_::_::c::[] -> (match a with
		| Conquered _ -> (match b with
			| Conquered _ -> (match c with
				| Conquered _ -> true
				| _ -> false
			)
			| _ -> false
		)
		| _ -> false
	)
	| _ -> false

let three_row board = check_v board || check_h board || check_d_a board || check_d_b board

let check_win board = is_full board || three_row board

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

let update board player = if check_win board then Conquered player else Board board

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

let getLineSizeChar order = (8 * (int_of_float(3.0 ** float_of_int (order - 1))))
let getNumberLinesChar order = (4 * int_of_float (3.0 ** float_of_int(order - 1)))

let getLineSizePiece order = int_of_float (3.0 ** float_of_int (order))

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

let displayOneCase (x, y) board order =
	let move = getMovesOfPixel (x, y) order in
	match getCase board move with
		| Conquered player -> " " ^ Player.toString player
		| _ -> " -"

let displayWithBoarder (x, y) board order =
	displayOneCase (x, y) board order ^ " |"

let toString board order =
	let max = getLineSizePiece order in
	let rec loop acc (x, y) max = match (x, y) with
		| (x, y) when x >= max && y >= max - 1 ->
			acc ^ "\n"
		| (x, y) when (y + 1) mod 3 = 0 && x >= max ->
			loop (acc ^ "\n" ^ String.make (getLineSizeChar order) '-' ^ "\n") (0, y + 1) max
		| (x, y) when x >= max ->
			loop (acc ^ "\n") (0, y + 1) max
		| (x, y) when (x + 1) mod 3 = 0 ->
			loop (acc ^ displayWithBoarder (x, y) board order) (x + 1, y) max
		| (x, y) ->
			loop (acc ^ displayOneCase (x, y) board order) (x + 1, y) max
	in loop "" (0, 0) max
