let read_input () = Aoc.stdin |> Seq.map int_of_string |> List.of_seq

let count_increases =
  let rec count_increases' acc = function
    | [] | [ _ ] -> acc
    | first :: (second :: _ as tl) when second > first ->
        count_increases' (acc + 1) tl
    | _ :: tl -> count_increases' acc tl
  in
  count_increases' 0

let () = read_input () |> count_increases |> Printf.printf "%d\n"
