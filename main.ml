let () =
        let (player_a, player_b, size, board) = Parsing.init in
	(*let player_a = (Color.Blue, "A", Player.Robot) in
	let player_b = (Color.Red, "B", Player.Robot) in
	let board = Board.newBoard 3 in*)
        (*let rec loop1 l = match l with
                | [] -> ""
                | (x::xs) -> print_int x; loop1 xs
        in
       let _ = loop1 (Parsing.givenPosition size) in*)
       let rec loop x boardx =
               print_endline (Board.toString boardx size);
               match Board.checkWin boardx with
               | Some a -> ()
               | None -> begin
               if x mod 2 = 0 then
                        match Board.play boardx player_a (Parsing.givenPosition size) with
                        | Ok a -> loop (x + 1) a
                        | Error b -> begin print_endline b; loop x boardx end
                else
                        match Board.play boardx player_b (Parsing.givenPosition size) with
                        | Ok a -> loop (x + 1) a
                        | Error b -> begin print_endline b; loop x boardx end
               end
        in
        loop 0 board
	(*let board_a = Board.play board player_a [3 ; 8 ; 5] in
	let board_b = Board.play board_a player_b [1 ; 4 ; 6] in
	print_endline (Board.toString board 3)
	; print_endline (Board.toString board_a 3)
	; print_endline (Board.toString board_b 3)*)
