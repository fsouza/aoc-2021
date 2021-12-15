open StdLabels
open MoreLabels

module Pos = struct
  type t = int * int

  let compare (row1, col1) (row2, col2) =
    let row = Int.compare row1 row2 in
    if row <> 0 then row else Int.compare col1 col2
end

module PosMap = Map.Make (Pos)
module PosHeap = Min_heap.Make (Pos)

type grid = { rows : int; map : int PosMap.t }

let int_of_char ch = Char.code ch - Char.code '0'

let add_to_grid { map; _ } (row_idx, row) =
  let rows = String.length row in
  let map =
    row
    |> String.to_seqi
    |> Seq.fold_left
         (fun map (col_idx, col) ->
           PosMap.add ~key:(row_idx, col_idx) ~data:(int_of_char col) map)
         map
  in
  { map; rows }

(* Note: can make this fast with a priorityqueue. Do we need it for part 2? *)
let distance (row_orig, col_orig) (row_dest, col_dest) =
  abs (row_dest - row_orig) + abs (col_dest - col_orig)

let neighbor_positions (row, col) rows =
  [ (row - 1, col); (row, col + 1); (row + 1, col); (row, col - 1) ]
  |> List.filter ~f:(fun (row, col) ->
         row >= 0 && row < rows && col >= 0 && col < rows)

let non_zero_mod x y =
  let v = x mod y in
  if v = 0 then y else v

let get_virtual_value { rows; map } (row, col) =
  (* Note: this function doesn't check bounds, if you ask for the value at
     position (1e9, 1e9) it'll do the math like if the grid was that large *)
  let row_add = row / rows in
  let col_add = col / rows in
  let row = row mod rows in
  let col = col mod rows in
  non_zero_mod (PosMap.find (row, col) map + row_add + col_add) 9

let find_path get_value rows grid =
  (* somewhat slow A*, adapted from
     https://en.wikipedia.org/wiki/A*_search_algorithm#Pseudocode *)
  let rec find_path' dest open_set g_score =
    match PosHeap.poll open_set with
    | None -> None
    | Some (v, open_set) ->
        if Pos.compare dest v = 0 then PosMap.find_opt dest g_score
        else
          let neighbor_positions = neighbor_positions v rows in
          let open_set, g_score =
            List.fold_left ~init:(open_set, g_score)
              ~f:(fun (open_set, g_score) neighbor_pos ->
                let neighbor_score =
                  PosMap.find_opt neighbor_pos g_score
                  |> Option.value ~default:max_int
                in
                let tentative_gscore =
                  PosMap.find v g_score + get_value grid neighbor_pos
                in
                if tentative_gscore >= neighbor_score then (open_set, g_score)
                else
                  let dist_to_dest = distance neighbor_pos dest in
                  let priority = tentative_gscore + dist_to_dest in
                  ( PosHeap.upsert ~key:neighbor_pos ~priority open_set,
                    PosMap.add ~key:neighbor_pos ~data:tentative_gscore g_score
                  ))
              neighbor_positions
          in
          find_path' dest open_set g_score
  in
  let start = (0, 0) in
  let dest = (rows - 1, rows - 1) in
  let g_score = PosMap.singleton start 0 in
  let priority = distance start dest in
  let open_set =
    PosHeap.create ~capacity:10 () |> PosHeap.upsert ~key:start ~priority
  in
  find_path' dest open_set g_score

let () =
  let grid =
    Aoc.zip Aoc.nat Aoc.stdin
    |> Seq.fold_left add_to_grid { rows = 0; map = PosMap.empty }
  in
  find_path (fun { map; _ } pos -> PosMap.find pos map) grid.rows grid
  |> Option.value ~default:max_int
  |> Printf.printf "Part 1: %d\n";
  find_path get_virtual_value (grid.rows * 5) grid
  |> Option.value ~default:max_int
  |> Printf.printf "Part 2: %d\n"
