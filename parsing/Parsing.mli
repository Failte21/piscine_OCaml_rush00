type tInit = (Player.t * Player.t * int * Board.t)
type tInput = int list

val init : tInit (*Ask user all informations*)

val givenPosition : int -> int list (*Ask player for a position*)

val askUser : string -> string (*Ask someting to the user*)
