type ('a, 'b) result =
	| Ok of 'a
	| Err of 'b

type t = Blue | Green | Red | White | Yellow | Cyan | Magenta

val toTermString : t -> string
val toString : t -> string
val applyColorToString : t -> string -> string
val parseString : string -> (t, string) result