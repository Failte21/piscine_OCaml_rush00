type tInit = (Player.t * Player.t * int * Board.t)

val start : unit -> tInit (*Ask user all informations*)
val restart : Player.t -> Player.t -> int -> tInit (*Ask user all informations*)

val givenPosition : int -> int list (*Ask player for a position*)

val askUser : string -> string (*Ask someting to the user*)

val askYes: string -> bool
