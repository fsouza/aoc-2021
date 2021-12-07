open StdLabels

let solve crabs =
  let rec align_to fuel target = function
    | [] -> fuel
    | hd :: tl -> align_to (fuel + Int.abs (hd - target)) target tl
  in
  crabs
  |> List.map ~f:(fun crab -> align_to 0 crab crabs)
  |> List.fold_left ~init:max_int ~f:min

let () =
  read_line ()
  |> String.split_on_char ~sep:','
  |> List.map ~f:int_of_string
  |> solve
  |> Printf.printf "%d\n"
