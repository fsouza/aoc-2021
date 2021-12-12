open StdLabels
open MoreLabels
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

let parse line =
  match String.split_on_char ~sep:'-' line with
  | [ orig; dest ] -> Some (orig, dest)
  | _ -> None

let add_to_map map (orig, dest) =
  let curr_orig = StringMap.find_opt orig map |> Option.value ~default:[] in
  let curr_dest = StringMap.find_opt dest map |> Option.value ~default:[] in
  map
  |> StringMap.add ~key:orig ~data:(dest :: curr_orig)
  |> StringMap.add ~key:dest ~data:(orig :: curr_dest)

let is_big_cave cave = String.uppercase_ascii cave = cave

let count_paths target map =
  let rec path_from' visited origin =
    if origin = target then 1
    else
      StringMap.find origin map
      |> List.filter ~f:(fun dest -> not (StringSet.mem dest visited))
      |> List.fold_left ~init:0 ~f:(fun acc connection ->
             let visited =
               if is_big_cave connection then visited
               else StringSet.add connection visited
             in
             acc + path_from' visited connection)
  in
  path_from' (StringSet.singleton "start") "start"

let () =
  Aoc.stdin
  |> Seq.filter_map parse
  |> Seq.fold_left add_to_map StringMap.empty
  |> count_paths "end"
  |> Printf.printf "%d\n"
