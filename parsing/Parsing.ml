type tInit = (Player.t * Player.t * int * Board.t)
type tInput = int list

let is_digit = function '0' .. '9' -> true | _ -> false
let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_ascii s =
        if is_digit s.[0] || is_alpha s.[0] then true
        else false

let is_ok res = match res with
        | Ok a -> true
        | Error b -> false

let get_ok res = match res with
        | Ok a -> a
        | Error b -> invalid_arg "get_ok Err x"

let get_error res = match res with
        | Ok a -> invalid_arg "get_error OK x"
        | Error b -> b

let parseString s = match String.split_on_char ' ' s with
        | letter :: color -> begin match (String.length letter = 1 && is_ascii letter, color) with
                | (_, []) | (_, (_::_::_)) | (false, _)-> Error ("Wrong input : \"" ^ s ^ "\"")
                | (true, (x::xs)) -> begin match Player.newPlayer x letter "Human" with
                        | Ok a -> Ok a
                        | Error b -> Error ("Wrong input : \"" ^ s ^ "\"")
                        end
                end
        | invalid -> Error ("Wrong input : \"" ^ s ^ "\"")

let ft_string_all fct str =
        let rec loop len =
                if len >= 0 then begin
                        if fct (str.[len]) == false then
                                false
                        else
                                loop (len - 1)
                end
                        else
                                true
        in
        loop (String.length str - 1)

let askUser s =
        if s <> "" then begin print_endline s; read_line() end
        else read_line()

let rec matchYesNo ask ans = match String.lowercase_ascii (String.trim ans) with
                | "yes" | "y" -> true
                | "no" | "n" -> false
                | _ -> print_endline ("Wrong input: \"" ^ ans ^ "\""); matchYesNo ask (askUser ask)

let askYes s = matchYesNo s (askUser s)

let alreadyTaken player player1 =
        if is_ok player1 = false then false
                else begin
                        let pSet = get_ok player1 in
                        let p = get_ok player in
                        if Player.playerColor p = Player.playerColor pSet then begin
                                print_endline "Wrong input: Color already taken by player 1";
                                true end
        else if Player.playerName p = Player.playerName pSet then begin
                print_endline "Wrong input: Name already taken by player 1";
                                true end
                        else false
        end

let createIa pSet =
        if is_ok pSet = false then (Color.Red, "O", Player.Robot)
        else begin
                let ps = get_ok pSet in
                let name = if Player.playerName ps = "O" then "X" else "O" in
                let color = if Player.playerColor ps = Color.Red then Color.Blue else Color.Red in
                (color, name, Player.Robot)
                end

let newPlayer ask playerSet =
        if askYes ask = false then createIa playerSet
               else begin let rec loop player =
                       if is_ok player = false then begin
                               print_endline (get_error player);
                                loop (parseString (askUser "Give me his name and his color: <Letter> <Color>"))
        end
        else if alreadyTaken player playerSet = true then
                loop (parseString (askUser "Give me his name and his color: <Letter> <Color>"))
                       else get_ok player
                in
                loop (parseString (askUser "Give me his name and his color: <Letter> <Color>"))
                       end

let checkInt _ =
        let rec loop s =
                if (ft_string_all is_digit s) = false || String.length s <> 1 || int_of_string s > 5 || s = "0" then
                        begin
                                print_endline ("Wrong input \"" ^ s ^ "\"");
                                loop (askUser "What is the size of the map ? <1..5>")
               end
        else int_of_string s
               in
                loop (askUser "What is the size of the map ? <1..5>")

let quickStart _ = (get_ok (Player.newPlayer "Red" "O" ""), get_ok (Player.newPlayer "Blue" "X" ""), 2, Board.newBoard 2)

let start _ =
        if askYes "Quick start ? <Yes,No>" = true then quickStart()
        else begin
        let size = checkInt() in
        let p1 = newPlayer "Is the first player Human ? <Yes,No>" (Error("No Player set")) in
        let p2 = newPlayer "Is the second player Human ? <Yes,No>" (Ok(p1)) in
        (p1, p2, size, Board.newBoard size)
        end

let restart pa pb size =
        if askYes "Keep same size ? <Yes,No>" = true then (pa, pb, size, Board.newBoard size)
        else begin
                let size = checkInt() in
                (pa, pb, size, Board.newBoard size)
        end


let rec givenPosition size =
        let s = askUser "Type position: <1..9> .." in
        let rec loop ix is =
                if (String.length ix > 1 || ft_string_all is_digit ix = false) then begin
                        print_endline "Wrong Input";
                        givenPosition size end
                else match is with
                        | [] -> [int_of_string ix]
                        | (x::xs) -> [int_of_string ix] @ loop x xs
        in
        match (String.split_on_char ' ' s, String.length s = 2 * size - 1) with
                | ([], _) -> begin print_endline "Wrong input"; givenPosition size end
                | (_, false) -> begin print_endline ("Wrong Deapth, please give me " ^ string_of_int size ^ " numbers"); givenPosition size end
                | ((x::xs), _) -> loop x xs
