open StdLabels
open MoreLabels

module Pos = struct
  type t = int * int

  let compare (row1, col1) (row2, col2) =
    let row = Int.compare row1 row2 in
    if row <> 0 then row else Int.compare col1 col2
end

module PosMap = Map.Make (Pos)
module PosSet = Set.Make (Pos)

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

let next_pos open_set f_score =
  let lowest_pos, _ =
    PosSet.fold
      ~init:((-1, -1), max_int)
      ~f:(fun pos ((_, lowest_f_score) as acc) ->
        let f_score =
          PosMap.find_opt pos f_score |> Option.value ~default:max_int
        in
        if f_score < lowest_f_score then (pos, f_score) else acc)
      open_set
  in
  (lowest_pos, PosSet.remove lowest_pos open_set)

let neighbor_positions (row, col) rows =
  [ (row - 1, col); (row, col + 1); (row + 1, col); (row, col - 1) ]
  |> List.filter ~f:(fun (row, col) ->
         row >= 0 && row < rows && col >= 0 && col < rows)

let non_zero_mod x y =
  let v = x mod y in
  if v = 0 then y else v

let get_virtual_value { rows; map } (row, col) =
  let row_add = row / rows in
  let col_add = col / rows in
  let row = row mod rows in
  let col = col mod rows in
  non_zero_mod (PosMap.find (row, col) map + row_add + col_add) 9

let find_path { rows; map } =
  (* somewhat slow A*, adapted from
     https://en.wikipedia.org/wiki/A*_search_algorithm#Pseudocode *)
  let virtual_rows = rows * 5 in
  let rec find_path' dest open_set g_score f_score =
    if PosSet.is_empty open_set then None
    else
      let v, open_set = next_pos open_set f_score in
      if Pos.compare dest v = 0 then PosMap.find_opt dest g_score
      else
        let neighbor_positions = neighbor_positions v virtual_rows in
        let open_set, g_score, f_score =
          List.fold_left
            ~init:(open_set, g_score, f_score)
            ~f:(fun (open_set, g_score, f_score) neighbor_pos ->
              let neighbor_score =
                PosMap.find_opt neighbor_pos g_score
                |> Option.value ~default:max_int
              in
              let tentative_gscore =
                PosMap.find v g_score
                + get_virtual_value { rows; map } neighbor_pos
              in
              if tentative_gscore >= neighbor_score then
                (open_set, g_score, f_score)
              else
                let dist_to_dest = distance neighbor_pos dest in
                ( PosSet.add neighbor_pos open_set,
                  PosMap.add ~key:neighbor_pos ~data:tentative_gscore g_score,
                  PosMap.add ~key:neighbor_pos
                    ~data:(tentative_gscore + dist_to_dest)
                    f_score ))
            neighbor_positions
        in
        find_path' dest open_set g_score f_score
  in
  let start = (0, 0) in
  let dest = (virtual_rows - 1, virtual_rows - 1) in
  let g_score = PosMap.singleton start 0 in
  let f_score = PosMap.singleton start (distance start dest) in
  let open_set = PosSet.singleton start in
  find_path' dest open_set g_score f_score

let () =
  Aoc.zip Aoc.nat Aoc.stdin
  |> Seq.fold_left add_to_grid { rows = 0; map = PosMap.empty }
  |> find_path
  |> Option.value ~default:max_int
  |> Printf.printf "%d\n"
