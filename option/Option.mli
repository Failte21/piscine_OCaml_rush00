type 'a option =
	| Some of 'a
	| None

val flatMap : ('a option -> 'a option) -> 'a option

val map : ('a -> 'a) -> 'a option

val fold : ('a option -> 'b) -> ('a option -> 'b) -> 'a option