let flatMap fn result =
  match result with
    | Ok el -> fn el
    | e -> e

let map fn result =
  match result with
    | Ok el -> Ok (fn el)
    | e -> e

let fold on_success on_error result =
  match result with
    | Ok el -> on_success el
    | Error err -> on_error err