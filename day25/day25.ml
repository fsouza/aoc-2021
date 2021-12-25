open StdLabels
open MoreLabels

type cucumber = East | South

let parse_cucumber = function
  | '>' -> Some East
  | 'v' -> Some South
  | _ -> None

module PosSet = Set.Make (struct
  type t = int * int

  let compare (row1, col1) (row2, col2) =
    let row = Int.compare row1 row2 in
    if row != 0 then row else Int.compare col1 col2
end)

type state = {
  width : int;
  height : int;
  east_grid : PosSet.t;
  south_grid : PosSet.t;
}

let initial_state =
  { width = 0; height = 0; east_grid = PosSet.empty; south_grid = PosSet.empty }

let add_to_state ({ width; height; _ } as state) (row_idx, row) =
  let width = max (String.length row) width in
  let height = max height (row_idx + 1) in
  row
  |> String.to_seqi
  |> Seq.fold_left
       (fun ({ east_grid; south_grid; _ } as state) (col_idx, col) ->
         match parse_cucumber col with
         | None -> state
         | Some East ->
             { state with east_grid = PosSet.add (row_idx, col_idx) east_grid }
         | Some South ->
             {
               state with
               south_grid = PosSet.add (row_idx, col_idx) south_grid;
             })
       { state with width; height }

let neighbor_positions { width; height; _ } (row, col) =
  ((row, (col + 1) mod width), ((row + 1) mod height, col))

let try_move ~pos ~cucumber ({ east_grid; south_grid; _ } as state) =
  let east_neighbor, south_neighbor = neighbor_positions state pos in
  let neighbor =
    match cucumber with
    | East -> east_neighbor
    | South -> south_neighbor
  in
  if PosSet.mem neighbor east_grid || PosSet.mem neighbor south_grid then
    (state, false)
  else
    match cucumber with
    | East ->
        let east_grid = east_grid |> PosSet.remove pos |> PosSet.add neighbor in
        ({ state with east_grid }, true)
    | South ->
        let south_grid =
          south_grid |> PosSet.remove pos |> PosSet.add neighbor
        in
        ({ state with south_grid }, true)

let step ({ east_grid; _ } as state) =
  let ({ south_grid; _ } as state), moved =
    east_grid
    |> PosSet.fold ~init:(state, false) ~f:(fun pos (state, moved_acc) ->
           let state, moved = try_move ~pos ~cucumber:East state in
           (state, moved || moved_acc))
  in
  south_grid
  |> PosSet.fold ~init:(state, moved) ~f:(fun pos (state, moved_acc) ->
         let state, moved = try_move ~pos ~cucumber:South state in
         (state, moved || moved_acc))

let print_grid { width; height; east_grid; south_grid } =
  for row = 0 to height - 1 do
    for col = 0 to width - 1 do
      let pos = (row, col) in
      let ch =
        if PosSet.mem pos east_grid then '>'
        else if PosSet.mem pos south_grid then 'v'
        else '.'
      in
      print_char ch
    done;
    print_newline ()
  done;
  print_newline ();
  flush stdout

let count_steps_until_freeze state =
  let rec count_steps_until_freeze steps (state, moved) =
    Printf.printf "After %d steps:\n" steps;
    print_grid state;
    flush stdout;
    if not moved then steps
    else
      let state, moved = step state in
      count_steps_until_freeze (steps + 1) (state, moved)
  in
  count_steps_until_freeze 0 (state, true)

let () =
  Aoc.zip Aoc.nat Aoc.stdin
  |> Seq.fold_left add_to_state initial_state
  |> count_steps_until_freeze
  |> Printf.printf "%d\n"
