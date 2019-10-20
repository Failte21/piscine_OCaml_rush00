type ('a, 'b) result =
	| Ok of 'a
	| Err of 'b

(* val resultFlatMap : (('a, 'b) result -> ('a, 'b) result) -> ('a, 'b) result *)
