let () =
	let player_a = (Color.Blue, "A", Board.Player.Robot) in
	let player_b = (Color.Red, "B", Board.Player.Robot) in
	let board = Board.newBoard 3 in
	let board_a = Board.play board player_a [3 ; 8 ; 5] in
	let board_b = Board.play board_a player_b [1 ; 4 ; 6] in
	print_endline (Board.toString board 3)
	; print_endline (Board.toString board_a 3)
	; print_endline (Board.toString board_b 3)
