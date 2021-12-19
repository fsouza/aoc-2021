open StdLabels

let part1 snailfishes =
  snailfishes
  |> Lib.sum_elements
  |> Lib.magnitude
  |> Printf.printf "Part 1: %d\n"

let all_pairs el =
  let rec all_pairs' acc = function
    | [] -> acc
    | hd :: tl -> all_pairs' ((el, hd) :: (hd, el) :: acc) tl
  in
  all_pairs' []

let part2 snailfishes =
  snailfishes
  |> List.concat_map ~f:(fun snailfish -> all_pairs snailfish snailfishes)
  |> List.fold_left ~init:min_int ~f:(fun acc (f1, f2) ->
         let magnitude = Lib.add f1 f2 |> Lib.magnitude in
         max magnitude acc)
  |> Printf.printf "Part 2: %d\n"

let () =
  let snailfishes =
    Aoc.stdin |> Seq.map Lib.parse |> Seq.map Lib.reduce |> List.of_seq
  in
  part1 snailfishes;
  part2 snailfishes
