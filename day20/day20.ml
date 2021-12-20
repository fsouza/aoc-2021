open StdLabels
open MoreLabels

module Pos = struct
  type t = int * int

  let compare (row1, col1) (row2, col2) =
    let row_cmp = Int.compare row1 row2 in
    if row_cmp = 0 then Int.compare col1 col2 else row_cmp
end

module IntSet = Set.Make (Int)
module PosSet = Set.Make (Pos)

let value_of_pixel = function
  | '#' -> 1
  | _ -> 0

let int_of_pixels =
  let rec int_of_pixels' acc = function
    | [] -> acc
    | hd :: tl -> int_of_pixels' ((acc * 2) + value_of_pixel hd) tl
  in
  int_of_pixels' 0

type image = { grid : PosSet.t; width : int; height : int; infinite : char }

let initial_image =
  { grid = PosSet.empty; width = 0; height = 0; infinite = '.' }

let enhance algorithm { grid; infinite; width; height } = failwith "TODO"

let add_to_image image (row_idx, row) =
  row
  |> String.to_seqi
  |> Seq.fold_left
       (fun ({ grid; width; height; _ } as image) (col_idx, col) ->
         let grid =
           if Char.equal col '#' then PosSet.add (row_idx, col_idx) grid
           else grid
         in
         let width = max width (String.length row) in
         let height = max height (row_idx + 1) in
         { image with grid; width; height })
       image

let count_lit_pixels { grid; infinite; _ } =
  if infinite = '#' then
    failwith
      "can't count infinite lit pixels, image is probably in an invalid state"
  else PosSet.cardinal grid

let () =
  let algorithm = read_line () |> String.to_seq |> Array.of_seq in
  read_line () |> ignore;
  let image =
    Aoc.zip Aoc.nat Aoc.stdin |> Seq.fold_left add_to_image initial_image
  in
  image
  |> enhance algorithm
  |> enhance algorithm
  |> count_lit_pixels
  |> Printf.printf "%d\n"
