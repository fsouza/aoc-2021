open Aoc

let () =
  stdin
  |> Seq.map int_of_string
  |> Seq.fold_left ( + ) 0
  |> Printf.printf "%d\n"
