let () =
	let player = (Board.Color.Blue, "A", Board.Player.Robot) in
	let board = Board.Free in
	print_endline (Board.toString board 3)
	; print_endline (Board.toString board 2)
	; print_endline (Board.toString board 1)
