open StdLabels
open MoreLabels

module Pos = struct
  type t = int * int

  let compare (row1, col1) (row2, col2) =
    let row = Int.compare row1 row2 in
    if row != 0 then row else Int.compare col1 col2
end

module PosMap = Map.Make (Pos)
module PosSet = Set.Make (Pos)

let int_of_char ch = Char.code ch - Char.code '0'

let add_to_map map (row_idx, row) =
  row
  |> String.to_seqi
  |> Seq.fold_left
       (fun map (col_idx, col) ->
         PosMap.add ~key:(row_idx, col_idx) ~data:(int_of_char col) map)
       map

let neighbor_positions row col =
  [ (row, col - 1); (row - 1, col); (row, col + 1); (row + 1, col) ]

let neighbors row col heightmap =
  neighbor_positions row col
  |> List.fold_left ~init:[] ~f:(fun acc pos ->
         match PosMap.find_opt pos heightmap with
         | None -> acc
         | Some v -> v :: acc)

let find_basin row col visited heightmap =
  let rec find_basin' acc visited = function
    | [] -> Some (acc, visited)
    | pos :: tl when PosSet.mem pos visited -> find_basin' acc visited tl
    | pos :: _ when PosMap.mem pos heightmap && PosMap.find pos heightmap = 9 ->
        None
    | ((row, col) as pos) :: tl ->
        let neighbors_positions =
          neighbor_positions row col
          |> List.filter ~f:(fun pos ->
                 match PosMap.find_opt pos heightmap with
                 | Some 9 | None -> false
                 | Some _ -> true)
          |> List.filter ~f:(fun pos -> not (PosSet.mem pos visited))
        in
        find_basin' (acc + 1) (PosSet.add pos visited) (tl @ neighbors_positions)
  in
  find_basin' 0 visited [ (row, col) ]

let () =
  let heightmap =
    Aoc.zip Aoc.nat Aoc.stdin |> Seq.fold_left add_to_map PosMap.empty
  in
  heightmap
  |> PosMap.to_seq
  |> Seq.map fst
  |> Seq.fold_left
       (fun ((visited, (first, second, third)) as acc) (row, col) ->
         match find_basin row col visited heightmap with
         | None -> acc
         | Some (v, visited) ->
             if v > first then (visited, (v, first, second))
             else if v > second then (visited, (first, v, second))
             else if v > third then (visited, (first, second, v))
             else (visited, (first, second, third)))
       (PosSet.empty, (0, 0, 0))
  |> (fun (_, (first, second, third)) -> first * second * third)
  |> Printf.printf "%d\n"
