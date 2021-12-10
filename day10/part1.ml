open StdLabels

let matching = function
  | ')' -> '('
  | ']' -> '['
  | '}' -> '{'
  | '>' -> '<'
  | ch -> ch |> Printf.sprintf "don't know what to do with '%c'" |> invalid_arg

let eval input =
  let open Seq in
  let rec eval' stack seq =
    match seq () with
    | Nil -> Ok stack
    | Cons (ch, rest) -> (
        match ch with
        | ('(' | '[' | '{' | '<') as ch -> eval' (ch :: stack) rest
        | (')' | ']' | '}' | '>') as ch -> (
            let opening = matching ch in
            match stack with
            | hd :: tl when Char.equal hd opening -> eval' tl rest
            | _ -> Error ch)
        | _ -> eval' stack rest)
  in
  eval' [] (String.to_seq input)

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
