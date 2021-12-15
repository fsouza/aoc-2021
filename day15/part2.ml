open StdLabels
open MoreLabels

module PosMap = Map.Make (struct
  type t = int * int

  let compare (row1, col1) (row2, col2) =
    let row = Int.compare row1 row2 in
    if row != 0 then row else Int.compare col1 col2
end)

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

let non_zero_mod x y =
  let v = x mod y in
  if v = 0 then y else v

let get_virtual_value { rows; map } (row, col) =
  let row_add = row / rows in
  let col_add = col / rows in
  let row = row mod rows in
  let col = col mod rows in
  non_zero_mod (PosMap.find (row, col) map + row_add + col_add) 9

let find_path ({ rows; map } as grid) =
  let virtual_rows = rows * 5 in
  Printf.printf "virtual rows: %d\n" virtual_rows;
  let cache = Hashtbl.create rows in
  let rec cached_find_path' pos =
    match Hashtbl.find_opt cache pos with
    | None ->
        let result = find_path' pos in
        Hashtbl.add ~key:pos ~data:result cache;
        result
    | Some result -> result
  and find_path' = function
    | row, col when row = virtual_rows - 1 && col = virtual_rows - 1 ->
        get_virtual_value grid (row, col)
    | row, col when row >= virtual_rows || col >= virtual_rows -> max_int
    | (row, col) as pos ->
        let v = get_virtual_value grid pos in
        let right = cached_find_path' (row, col + 1) in
        let down = cached_find_path' (row + 1, col) in
        v + min right down
  in
  let start_position = (0, 0) in
  let v_start = PosMap.find start_position map in
  cached_find_path' start_position - v_start

let print_grid ({ rows; _ } as grid) =
  let virtual_rows = rows * 5 in
  for i = 0 to virtual_rows - 1 do
    if i mod rows = 0 then print_newline ();
    for j = 0 to virtual_rows - 1 do
      if j mod rows = 0 then print_string " ";
      Printf.printf "%d" (get_virtual_value grid (i, j))
    done;
    print_newline ()
  done

let () =
  let grid =
    Aoc.zip Aoc.nat Aoc.stdin
    |> Seq.fold_left add_to_grid { rows = 0; map = PosMap.empty }
  in
  print_grid grid;
  grid |> find_path |> Printf.printf "%d\n"
