open StdLabels

let sum_ap n = n * (n + 1) / 2

let solve calc_fn crabs =
  let rec align_to fuel target = function
    | [] -> fuel
    | hd :: tl -> align_to (fuel + calc_fn (Int.abs (hd - target))) target tl
  in
  crabs
  |> List.map ~f:(fun crab -> align_to 0 crab crabs)
  |> List.fold_left ~init:max_int ~f:min

let () =
  let run_part2 = ref false in
  Arg.parse_argv Sys.argv
    [ ("-part2", Arg.Set run_part2, "run part 2") ]
    (Fun.const ()) "AoC 2021 Day 7";
  let calc_fn = if !run_part2 then sum_ap else Fun.id in
  read_line ()
  |> String.split_on_char ~sep:','
  |> List.map ~f:int_of_string
  |> solve calc_fn
  |> Printf.printf "%d\n"
