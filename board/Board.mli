(* val resultFlatMap : (('a, 'b) result -> ('a, 'b) result) -> ('a, 'b) result *)
type t =
	| Board of t list
	| Conquered of Player.t
	| Free

type move = int list

val toString : t -> int -> string

val getLineSizePiece : int -> int
val getCase : t -> move -> t

val checkWin : t -> Player.t option
val play: t -> Player.t -> move -> t
val newBoard : int -> t
