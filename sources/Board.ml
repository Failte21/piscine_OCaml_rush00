type ('a, 'b) result =
	| Ok of 'a
	| Err of 'b

type 'a option =
	| Some of 'a
	| None

module Color =
struct
	type t = Blue | Green | Red | White | Yellow | Cyan | Magenta

	let toTermString t = match t with
		| Red -> "\x1b[31m"
		| Green -> "\x1b[32m"
		| Yellow -> "\x1b[33m"
		| Blue -> "\x1b[34m"
		| Magenta -> "\x1b[35m"
		| Cyan -> "\x1b[36m"
		| White -> "\x1b[00m"

	let applyColorToString t s = toTermString t ^ s ^ toTermString White

	let toString t = match t with
		| Red -> applyColorToString Red "Red"
		| Green -> applyColorToString Green "Green"
		| Yellow -> applyColorToString Yellow "Yellow"
		| Blue -> applyColorToString Blue "Blue"
		| Magenta -> applyColorToString Magenta "Magenta"
		| Cyan -> applyColorToString Cyan "Cyan"
		| White -> applyColorToString White "White"

	let parseString s = match String.lowercase (String.trim s) with
		| "red" | "r" -> Ok Red
		| "green" | "g" -> Ok Green
		| "yellow" | "y" -> Ok Yellow
		| "blue" | "b" -> Ok Blue
		| "magenta" | "m" -> Ok Magenta
		| "cyan" | "c" -> Ok Cyan
		| "white" | "w" -> Ok White
		| invalid -> Err ("Wrong color name : \"" ^ invalid ^ "\"")
end

module Player =
struct
	type brain = Human | Robot
	type t = Color.t * string * brain

	let brainToString brain = match brain with
		| Human -> "Human"
		| Robot -> "Robot"

	let toString (color, s, _) = Color.applyColorToString color s
	let toStringVerbose (color, s, brain) = "Player " ^ toString (color, s, brain) ^ " (" ^ brainToString brain ^")"

end

type t =
	| Board of t list
	| Conquered of Player.t
	| Free

type move = int list

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
		| Conquered player -> " " ^ Player.toString player ^ " "
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
