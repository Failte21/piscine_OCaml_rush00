type 'a option =
	| Some of 'a
	| None

let flatMap fn result =
  match result with
    | Some el -> fn el
    | e -> e

let map fn result =
  match result with
    | Some el -> Some (fn el)
    | e -> e

let fold on_element on_empty result =
  match result with
    | Some el -> on_element el
    | None -> on_empty