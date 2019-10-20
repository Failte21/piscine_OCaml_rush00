type ('a, 'b) result =
	| Ok of 'a
	| Err of 'b

val resultFlatMap : (('a, 'b) result -> ('a, 'b) result) -> ('a, 'b) result

type 'a option =
	| Some of 'a
	| None

module Color :
sig
	type t = Blue | Green | Red | White | Yellow | Cyan | Magenta

	val toTermString : t -> string
	val toString : t -> string
	val applyColorToString : t -> string -> string
	val parseString : string -> t
end

module Player :
sig
	type brain = Human | Robot
	type t = Color.t * string * brain

	(* val newPlayer : string -> string -> t *)

	val toString : t -> string
	val toStringVerbose : t -> string

	(* "A Blue" | "X Magenta" and checks if not already in player list -> Returns a result *)
	val parseString : string -> t list -> (t, string) result
end

type t =
	| Board of t list
	| Conquered of Player.t
	| Free

type move = int list

(*
val parseMove : string -> move

(* This is the AI motherfucker *)
val bestMove : Player.t -> t -> move

(* try to apply a move on t -> Returns Ok(t) if success of Err(err_string, t) if not *)
val applyMove : move -> case -> Player.t -> (t, string * t) result

(* check t, returns a possibly modified t if a player wins it (or one of its descendant)
	This function should recursivly apply to all descendant t
*)
val check : t -> (t * (Player.t option))
*)

val toString : t -> int -> string
val translate : move -> int

val getLineSizePiece : int -> int
val getCase : t -> move -> t

val checkWin : t -> Player.t option
val play: t -> Player.t -> move -> t
val newBoard : int -> t


(*
 - - - |
 - - - |
 - - - |
 -------

 - - - | - - - | - - - |
 - - - | - - - | - - - |
 - - - | - - - | - - - |
 --------------------- |
 - - - | - - - | - - - |
 - - - | - - - | - - - |
 - - - | - - - | - - - |
 --------------------- |
 - - - | - - - | - - - |
 - - - | - - - | - - - |
 - - - | - - - | - - - |
 -----------------------


 ------------------------>X (10)
|- - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - |
|- - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - |
|- - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - |
|--------------------- | --------------------- | --------------------- |
|- - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - |
|- - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - |
|- - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - |
|--------------------- | --------------------- | --------------------- |
|- - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - |
|- - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - |
|- - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - |
|--------------------------------------------------------------------- |
\- - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - |
\- - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - |
X- - - | - - - | - - - | - X - | - - - | - - - | - - - | - - - | - - - |
 --------------------- | --------------------- | --------------------- |
 - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - |
 - - - | - - - | - - - | - - - | - X - | - - - | - - - | - - - | - - - |
 - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - |
 --------------------- | --------------------- | --------------------- |
 - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - |
 - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - |
 - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - |
 --------------------------------------------------------------------- |
 - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - |
 - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - |
 - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - |
 --------------------- | --------------------- | --------------------- |
 - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - |
 - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - |
 - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - |
 --------------------- | --------------------- | --------------------- |
 - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - |
 - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - |
 - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - | - - - |
 --------------------------------------------------------------------- |

 *)
