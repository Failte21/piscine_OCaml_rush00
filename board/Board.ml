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

let bestMove player board =
	let rec browse board index = match board with
		| Free -> [index + 1]
		| Conquered player -> []
		| Board b_list ->
			let rec browse_in_list b_list index = match b_list with
				| hd::tl ->
					let result = browse hd index in
					if result = []
					then browse_in_list tl (index + 1)
					else result
				| [] -> []
			in (index + 1)::(browse_in_list b_list 0)
	in

	match (browse board 0) with
	| hd::tl -> tl
	| [] -> []
