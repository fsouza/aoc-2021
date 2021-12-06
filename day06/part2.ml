open StdLabels
open MoreLabels
module IntMap = Map.Make (Int)

let proc_day clock = if clock = 0 then [ 8; 6 ] else [ clock - 1 ]

let add_to_map ?(value = 1) map fish =
  let curr = IntMap.find_opt fish map |> Option.value ~default:0 in
  IntMap.add ~key:fish ~data:(curr + value) map

let run until =
  let rec run' day map =
    if day = until + 1 then map
    else
      run' (day + 1)
        (IntMap.fold ~init:IntMap.empty
           ~f:(fun ~key ~data map ->
             if key = 0 then
               let map = add_to_map ~value:data map 6 in
               add_to_map ~value:data map 8
             else add_to_map ~value:data map (key - 1))
           map)
  in
  run' 1

let () =
  read_line ()
  |> String.split_on_char ~sep:','
  |> List.map ~f:int_of_string
  |> List.fold_left ~init:IntMap.empty ~f:add_to_map
  |> run 256
  |> IntMap.fold ~f:(fun ~key:_ ~data acc -> acc + data) ~init:0
  |> Printf.printf "%d\n"
