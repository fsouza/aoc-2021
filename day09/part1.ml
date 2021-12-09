open StdLabels
open MoreLabels

module PosMap = Map.Make (struct
  type t = int * int

  let compare (row1, col1) (row2, col2) =
    let row = Int.compare row1 row2 in
    if row != 0 then row else Int.compare col1 col2
end)

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

let find_lowest_points heightmap =
  let is_lowest_point row col =
    let neighbors =
      neighbor_positions row col
      |> List.fold_left ~init:[] ~f:(fun acc pos ->
             match PosMap.find_opt pos heightmap with
             | None -> acc
             | Some v -> v :: acc)
    in
    PosMap.find_opt (row, col) heightmap
    |> Option.map (fun v -> (v, List.for_all ~f:(( < ) v) neighbors))
  in
  let rec find_lowest_points' acc row col =
    match is_lowest_point row col with
    | None -> (
        match PosMap.find_opt (row + 1, 0) heightmap with
        | None -> acc
        | Some _ -> find_lowest_points' acc (row + 1) 0)
    | Some (v, true) -> find_lowest_points' (v :: acc) row (col + 1)
    | Some (_, false) -> find_lowest_points' acc row (col + 1)
  in
  find_lowest_points' [] 0 0

let () =
  Aoc.zip Aoc.nat Aoc.stdin
  |> Seq.fold_left add_to_map PosMap.empty
  |> find_lowest_points
  |> List.map ~f:(( + ) 1)
  |> List.fold_left ~init:0 ~f:( + )
  |> Printf.printf "%d\n"
