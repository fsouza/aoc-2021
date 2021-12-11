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

let neighbor_positions (row, col) grid =
  [
    (row - 1, col - 1);
    (row - 1, col);
    (row - 1, col + 1);
    (row, col - 1);
    (row, col + 1);
    (row + 1, col - 1);
    (row + 1, col);
    (row + 1, col + 1);
  ]
  |> List.to_seq
  |> Seq.filter ((Fun.flip PosMap.mem) grid)

let flash grid =
  (* precondition: `to_flash` must contain the positions of items that are bigger than 9 *)
  let rec flash' to_flash flashed new_grid =
    match Queue.take_opt to_flash with
    | None -> (flashed, new_grid)
    | Some pos ->
        if PosSet.mem pos flashed then flash' to_flash flashed new_grid
        else
          let neighbors =
            neighbor_positions pos grid
            |> Seq.filter (fun pos -> not (PosSet.mem pos flashed))
            |> Seq.map (fun pos ->
                   let curr = PosMap.find pos new_grid in
                   (pos, curr + 1))
          in
          let neighbors_to_flash =
            neighbors
            |> Seq.filter_map (fun (pos, v) -> if v > 9 then Some pos else None)
          in
          Queue.add_seq to_flash neighbors_to_flash;
          flash' to_flash (PosSet.add pos flashed)
            (new_grid |> PosMap.add ~key:pos ~data:0 |> PosMap.add_seq neighbors)
  in
  let to_visit = Queue.create () in
  PosMap.fold ~init:(PosSet.empty, grid)
    ~f:(fun ~key:pos ~data (flashed, new_grid) ->
      if data > 9 then (
        Queue.add pos to_visit;
        flash' to_visit flashed new_grid)
      else (flashed, new_grid))
    grid

let step grid =
  let flashed, grid = grid |> PosMap.map ~f:(( + ) 1) |> flash in
  (grid, PosSet.cardinal flashed)

let print_grid step grid =
  (* this is ugly *)
  Printf.printf "After step %d:\n" step;
  let curr_row = ref 0 in
  grid
  |> PosMap.to_seq
  |> Seq.map fst
  |> List.of_seq
  |> List.sort ~cmp:Pos.compare
  |> List.iter ~f:(fun ((row, _) as pos) ->
         if row > !curr_row then (
           print_newline ();
           incr curr_row);
         print_int (PosMap.find pos grid));
  print_endline "\n";
  flush stdout

let run orig_n grid =
  print_grid 0 grid;
  let rec run' n_flashed n grid =
    if n = 0 then n_flashed
    else
      let grid, flashed = step grid in
      print_grid (orig_n - n + 1) grid;
      run' (n_flashed + flashed) (n - 1) grid
  in
  run' 0 orig_n grid

let () =
  Aoc.zip Aoc.nat Aoc.stdin
  |> Seq.fold_left add_to_map PosMap.empty
  |> run 100
  |> Printf.printf "%d\n"
