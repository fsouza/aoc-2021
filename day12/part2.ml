open StdLabels
open MoreLabels
module StringMap = Map.Make (String)

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

let can_visit cave visits =
  if is_big_cave cave then true
  else if cave = "start" then false
  else
    match StringMap.find_opt cave visits with
    | None -> true
    | Some _ -> StringMap.for_all ~f:(fun _ v -> v = 1) visits

let add_visit cave visits =
  let curr = StringMap.find_opt cave visits |> Option.value ~default:0 in
  StringMap.add ~key:cave ~data:(curr + 1) visits

let count_paths target map =
  let rec path_from' visits origin =
    if origin = target then 1
    else
      StringMap.find origin map
      |> List.filter ~f:(fun dest -> can_visit dest visits)
      |> List.fold_left ~init:0 ~f:(fun acc connection ->
             let visits =
               if is_big_cave connection then visits
               else add_visit connection visits
             in
             acc + path_from' visits connection)
  in
  path_from' StringMap.empty "start"

let () =
  Aoc.stdin
  |> Seq.filter_map parse
  |> Seq.fold_left add_to_map StringMap.empty
  |> count_paths "end"
  |> Printf.printf "%d\n"
