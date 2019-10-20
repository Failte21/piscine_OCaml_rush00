let () =
       let rec loop x (pa, pb, size, boardx) =
               print_endline (Board.toString boardx size);
               match Board.checkWin boardx with
               | Some a -> begin
                       print_endline (Player.toStringVerbose a ^ " Won !");
                       if Parsing.askYes "Play again ? <Yes,No>" then loop 0 (Parsing.restart pa pb size) else () end
               | None -> begin
               if x mod 2 = 0 then
                        match Board.play boardx pa (Parsing.givenPosition size) with
                        | Ok a -> loop (x + 1) (pa, pb, size, a)
                        | Error b -> begin print_endline b; loop x (pa, pb, size, boardx)end
                else
                        match Board.play boardx pb (Parsing.givenPosition size) with
                        | Ok a -> loop (x + 1) (pa, pb, size, a)
                        | Error b -> begin print_endline b; loop x (pa, pb, size, boardx)end
               end
        in
        loop 0 (Parsing.start())
