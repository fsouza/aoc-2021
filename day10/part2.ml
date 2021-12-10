open StdLabels

let matching = function
  | ')' -> '('
  | ']' -> '['
  | '}' -> '{'
  | '>' -> '<'
  | '(' -> ')'
  | '[' -> ']'
  | '{' -> '}'
  | '<' -> '>'
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
  | ')' -> 1
  | ']' -> 2
  | '}' -> 3
  | '>' -> 4
  | _ -> 0

let fix_stack stack =
  stack
  |> List.fold_left ~init:[] ~f:(fun acc ch ->
         let matching = matching ch in
         matching :: acc)
  |> List.rev

let score =
  List.fold_left ~init:0 ~f:(fun acc ch ->
      let points = points ch in
      (acc * 5) + points)

let () =
  Aoc.stdin
  |> Seq.map eval
  |> Seq.filter_map (function
       | Ok [] -> None
       | Ok stack -> Some stack
       | Error _ -> None)
  |> Seq.map fix_stack
  |> Seq.map score
  |> Array.of_seq
  |> (fun arr ->
       Array.sort ~cmp:Int.compare arr;
       arr.(Array.length arr / 2))
  |> Printf.printf "%d\n"
