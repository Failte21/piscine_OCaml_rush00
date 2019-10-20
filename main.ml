let () =
        (* let (player_a, player_b, size, board) = Parsing.init in *)
	let player_a = (Color.Blue, "A", Player.Robot) in
	let player_b = (Color.Red, "B", Player.Robot) in
	let board = Board.newBoard 1 in
  let rec loop x boardx =
    print_endline (Board.toString boardx 1);
    match Board.checkWin boardx with
      | Some a -> ()
      | None -> begin
        match boardx with
        | Board boards -> (
          if x mod 2 = 0 then
           let move = (Board.findBestMove boards player_a player_b) + 1 in
            match Board.play boardx player_a [move] with
            | Ok a -> loop (x + 1) a
            | Error b -> begin print_endline b; loop x boardx end
          else
            let move = (Board.findBestMove boards player_b player_a) + 1 in
            match Board.play boardx player_b [move] with
            | Ok a -> loop (x + 1) a
            | Error b -> begin print_endline b; loop x boardx end
        )
        | _ -> print_endline "..."
      end
  in
  loop 0 board
	(*let board_a = Board.play board player_a [3 ; 8 ; 5] in
	let board_b = Board.play board_a player_b [1 ; 4 ; 6] in
	print_endline (Board.toString board 3)
	; print_endline (Board.toString board_a 3)
	; print_endline (Board.toString board_b 3)*)
