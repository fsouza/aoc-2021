open StdLabels
open MoreLabels

module Pos = struct
  type t = int * int

  let compare (row1, col1) (row2, col2) =
    let row_cmp = Int.compare row1 row2 in
    if row_cmp = 0 then Int.compare col1 col2 else row_cmp
end

module PosSet = Set.Make (Pos)

let value_of_pixel = function
  | '#' -> 1
  | _ -> 0

let int_of_pixels =
  let open Seq in
  let rec int_of_pixels' acc seq =
    match seq () with
    | Nil -> acc
    | Cons (v, seq) -> int_of_pixels' ((acc * 2) + value_of_pixel v) seq
  in
  int_of_pixels' 0

type image = {
  grid : PosSet.t;
  row_bounds : int * int;
  col_bounds : int * int;
  infinite : char;
}

let initial_image =
  {
    grid = PosSet.empty;
    row_bounds = (0, 0);
    col_bounds = (0, 0);
    infinite = '.';
  }

let all_positions
    { row_bounds = min_row, max_row; col_bounds = min_col, max_col; _ } =
  let open Seq in
  let min_row = min_row - 2 in
  let max_row = max_row + 2 in
  let min_col = min_col - 2 in
  let max_col = max_col + 2 in
  let rec all_positions' row col () =
    if row > max_row then Nil
    else if col > max_col then all_positions' (row + 1) min_col ()
    else Cons ((row, col), all_positions' row (col + 1))
  in
  ((min_row, max_row), (min_col, max_col), all_positions' min_row min_col)

let is_inbound (min_row, max_row) (min_col, max_col) (row, col) =
  row >= min_row && row <= max_row && col >= min_col && col <= max_col

let get_pixel { grid; row_bounds; col_bounds; infinite } pos =
  if is_inbound row_bounds col_bounds pos then
    if PosSet.mem pos grid then '#' else '.'
  else infinite

let square_positions (center_row, center_col) =
  let open Seq in
  let rec square_positions' row col () =
    if row > center_row + 1 then Nil
    else if col > center_col + 1 then
      square_positions' (row + 1) (center_col - 1) ()
    else Cons ((row, col), square_positions' row (col + 1))
  in
  square_positions' (center_row - 1) (center_col - 1)

let get_index image pos =
  pos |> square_positions |> Seq.map (get_pixel image) |> int_of_pixels

let enhance_once algorithm ({ infinite; _ } as image) =
  let row_bounds, col_bounds, all_positions = all_positions image in
  let grid =
    all_positions
    |> Seq.fold_left
         (fun grid pos ->
           let idx = get_index image pos in
           let ch = algorithm.(idx) in
           if ch = '#' then PosSet.add pos grid else grid)
         PosSet.empty
  in
  let infinite_idx = value_of_pixel infinite * 9 in
  let infinite = algorithm.(infinite_idx) in
  { grid; row_bounds; col_bounds; infinite }

let add_to_image image (row_idx, row) =
  row
  |> String.to_seqi
  |> Seq.fold_left
       (fun ({
               grid;
               row_bounds = min_row, max_row;
               col_bounds = min_col, max_col;
               _;
             } as image) (col_idx, col) ->
         let grid =
           if Char.equal col '#' then PosSet.add (row_idx, col_idx) grid
           else grid
         in
         {
           image with
           grid;
           row_bounds = (min_row, max max_row row_idx);
           col_bounds = (min_col, max max_col col_idx);
         })
       image

let count_lit_pixels { grid; _ } = PosSet.cardinal grid

let rec enhance times algorithm image =
  if times = 0 then image
  else enhance (times - 1) algorithm (enhance_once algorithm image)

let part1 algorithm image =
  enhance 2 algorithm image |> count_lit_pixels |> Printf.printf "Part 1: %d\n"

let part2 algorithm image =
  enhance 50 algorithm image |> count_lit_pixels |> Printf.printf "Part 2: %d\n"

let () =
  let algorithm = read_line () |> String.to_seq |> Array.of_seq in
  read_line () |> ignore;
  let image =
    Aoc.zip Aoc.nat Aoc.stdin |> Seq.fold_left add_to_image initial_image
  in
  part1 algorithm image;
  part2 algorithm image
