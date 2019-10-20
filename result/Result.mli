type ('a, 'b) result =
	| Ok of 'a
	| Err of 'b

val flatMap : (('a, 'b) result -> ('a, 'b) result) -> ('a, 'b) result

val map : ('a -> 'a) -> ('a, 'b) result

val fold : (('a, 'b) result -> 'c) -> (('a, 'b) result -> 'c) -> ('a, 'b) result
