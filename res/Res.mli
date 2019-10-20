val flatMap : ('a -> ('a, 'b) result) -> ('a, 'b) result -> ('a, 'b) result

val map : ('a -> 'a) -> ('a, 'b) result -> ('a, 'b) result

val fold : ('a -> 'c) -> ('b -> 'c) -> ('a, 'b) result -> 'c
