type brain = Human | Robot
type t = Color.t * string * brain

val newPlayer : string -> string -> string -> (t, string) result

val toString : t -> string
val toStringVerbose : t -> string

val playerColor : t -> Color.t
val playerName : t -> string
(* "A Blue" | "X Magenta" and checks if not already in player list -> Returns a result *)
(* val parseString : string -> t list -> (t, string) result *)
