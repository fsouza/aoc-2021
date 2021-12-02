open StdLabels

let parse line =
  match String.split_on_char ~sep:' ' line with
  | [ "forward"; v ] -> (int_of_string v, 0)
  | [ "down"; v ] -> (0, int_of_string v)
  | [ "up"; v ] -> (0, -1 * int_of_string v)
  | _ -> line |> Printf.sprintf "invalid input '%s'" |> invalid_arg

let execute (horizontal, depth) (x, y) = (horizontal + x, depth + y)

let mult (horizontal, depth) = horizontal * depth

let () =
  Aoc.stdin
  |> Seq.map parse
  |> Seq.fold_left execute (0, 0)
  |> mult
  |> Printf.printf "%d\n"
