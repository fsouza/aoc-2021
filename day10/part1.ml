let matching = function
  | ')' -> '('
  | ']' -> '['
  | '}' -> '{'
  | '>' -> '<'
  | ch -> ch |> Printf.sprintf "don't know what to do with '%c'" |> invalid_arg

let eval input =
  let rec eval' stack idx =
    if idx = String.length input then Ok stack
    else
      match input.[idx] with
      | ('(' | '[' | '{' | '<') as ch -> eval' (ch :: stack) (idx + 1)
      | (')' | ']' | '}' | '>') as ch -> (
          let opening = matching ch in
          match stack with
          | hd :: tl when Char.equal hd opening -> eval' tl (idx + 1)
          | _ -> Error ch)
      | _ -> eval' stack (idx + 1)
  in
  eval' [] 0

let points = function
  | ')' -> 3
  | ']' -> 57
  | '}' -> 1197
  | '>' -> 25137
  | _ -> invalid_arg "invalid char"

let () =
  Aoc.stdin
  |> Seq.map eval
  |> Seq.filter_map (function
       | Error ch -> Some ch
       | _ -> None)
  |> Seq.map points
  |> Seq.fold_left ( + ) 0
  |> Printf.printf "%d\n"
