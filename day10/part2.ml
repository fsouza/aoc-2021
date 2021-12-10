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
