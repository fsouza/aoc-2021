open StdLabels

let proc_day clock = if clock = 0 then [ 8; 6 ] else [ clock - 1 ]

let run until =
  let rec run' day fishes =
    if day = until + 1 then fishes
    else run' (day + 1) (List.concat_map ~f:proc_day fishes)
  in
  run' 1

let () =
  read_line ()
  |> String.split_on_char ~sep:','
  |> List.map ~f:int_of_string
  |> run 80
  |> List.length
  |> Printf.printf "%d\n"
