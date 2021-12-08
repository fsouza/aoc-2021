open StdLabels

let parse_patterns input = input |> String.trim |> String.split_on_char ~sep:' '

let parse str =
  match String.split_on_char ~sep:'|' str with
  | [ patterns; output ] -> Some (parse_patterns patterns, parse_patterns output)
  | _ -> None

let find_1_4_7_8 (_, output) =
  output
  |> List.to_seq
  |> Seq.map String.length
  |> Seq.filter_map (function
       | 2 | 4 | 3 | 7 -> Some 1
       | _ -> None)
  |> Seq.fold_left ( + ) 0

let () =
  Aoc.stdin
  |> Seq.filter_map parse
  |> Seq.map find_1_4_7_8
  |> Seq.fold_left ( + ) 0
  |> Printf.printf "%d\n"
