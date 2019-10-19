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
		| White -> "\x1b[0m"

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

	let newPlayer letter color = match (String.length letter = 1 && is_ascii letter, Color.parseString color) with
		| ()

	let parseString s = match String.split_on_char ' ' s with
		| letter :: color -> newPlayer letter color
		| invalid -> Err ("Wrong input : \"" ^ s ^ "\"")

end

module Board =
struct
	type t =
		| Board of t list
		| Conquered of Player.t
		| Free

	type move = int list

end
