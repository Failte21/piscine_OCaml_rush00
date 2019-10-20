let () =
	let player_a = (Color.Blue, "A", Player.Robot) in
	(* let player_b = (Color.Red, "B", Player.Robot) in *)
	let board = Board.newBoard 3 in
	let board_a = Board.play board player_a [3 ; 8 ; 15] in
	(* let board_b = Res.flatMap (fun e -> Board.play e player_a [3 ; 8 ; 5]) board_a in *)
	(* let board_b = Res.flatMap (fun e -> Board.play e player_a [1 ; 4 ; 6]) board_a in *)
	(* let board_b = Board.play board_a player_b [1 ; 4 ; 6] in
  let board_c = Board.play board_b player_a [1 ; 4 ; 6] in *)
  Res.fold
    (fun b -> print_endline (Board.toString b 3))
    print_endline
    board_a
	(* print_endline (Board.toString board 3) *)
	(* ; print_endline (Board.toString board_a 3)
	; print_endline (Board.toString board_b 3) *)
  ; print_char '\n'
