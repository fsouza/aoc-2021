let () =
  Aoc.stdin
  |> Seq.map Lib.parse
  |> Seq.map Lib.reduce
  |> List.of_seq
  |> Lib.sum_elements
  |> Lib.magnitude
  |> Printf.printf "%d\n"