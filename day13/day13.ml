open StdLabels
open MoreLabels

module DotSet = Set.Make (struct
  type t = int * int

  let compare (x1, y1) (x2, y2) =
    let cmp = Int.compare x1 x2 in
    if cmp != 0 then cmp else Int.compare y1 y2
end)

type fold = Fold_x of int | Fold_y of int
type grid = { dots : DotSet.t; rows : int; columns : int }

let print_grid { dots; rows; columns } =
  for y = 0 to rows - 1 do
    for x = 0 to columns - 1 do
      let ch = if DotSet.mem (x, y) dots then '#' else '.' in
      print_char ch
    done;
    print_newline ();
    flush stdout
  done

let empty_grid = { dots = DotSet.empty; rows = 0; columns = 0 }

let parse_fold_pos pos =
  match String.split_on_char ~sep:'=' pos with
  | [ "x"; v ] -> Some (Fold_x (int_of_string v))
  | [ "y"; v ] -> Some (Fold_y (int_of_string v))
  | _ -> None

let parse_fold line =
  match String.split_on_char ~sep:' ' line with
  | [ "fold"; "along"; pos ] -> parse_fold_pos pos
  | _ -> None

let parse_into_state (({ dots; rows; columns } as grid), folds) line =
  match String.split_on_char ~sep:',' line with
  | [ x; y ] ->
      let x, y = (int_of_string x, int_of_string y) in
      let dots = DotSet.add (x, y) dots in
      let rows = max (y + 1) rows in
      let columns = max (x + 1) columns in
      ({ dots; rows; columns }, folds)
  | _ -> (
      match parse_fold line with
      | None -> (grid, folds)
      | Some fold -> (grid, fold :: folds))

let fold grid = function
  | Fold_y n ->
      let { dots; rows; _ } = grid in
      let rows_before_fold = n - 0 in
      let rows_after_fold = rows - n - 1 in
      let new_rows = max rows_before_fold rows_after_fold in
      let dots =
        DotSet.fold ~init:DotSet.empty
          ~f:(fun (x, y) dots ->
            if y = n then dots
            else if y < n then DotSet.add (x, y) dots
            else
              let delta = y - n in
              DotSet.add (x, n - delta) dots)
          dots
      in
      { grid with rows = new_rows; dots }
  | Fold_x n ->
      let { dots; columns; _ } = grid in
      let columns_before_fold = n - 0 in
      let columns_after_fold = columns - n - 1 in
      let new_columns = max columns_before_fold columns_after_fold in
      let dots =
        DotSet.fold ~init:DotSet.empty
          ~f:(fun (x, y) dots ->
            if x = n then dots
            else if x < n then DotSet.add (x, y) dots
            else
              let delta = x - n in
              DotSet.add (n - delta, y) dots)
          dots
      in
      { grid with columns = new_columns; dots }

let do_one_fold (grid, folds) =
  match folds with
  | [] -> invalid_arg "no folds available"
  | f :: _ -> fold grid f

let execute_folds (grid, folds) = List.fold_left ~init:grid ~f:fold folds

let part1 (grid, folds) =
  do_one_fold (grid, folds)
  |> (fun { dots; _ } -> DotSet.cardinal dots)
  |> Printf.printf "Part 1: %d\n"

let part2 (grid, folds) =
  execute_folds (grid, folds)
  |> fun grid ->
  print_newline ();
  print_endline "Part 2:";
  print_grid grid

let () =
  let grid, folds =
    Aoc.stdin
    |> Seq.fold_left parse_into_state (empty_grid, [])
    |> fun (grid, folds) -> (grid, List.rev folds)
  in
  part1 (grid, folds);
  part2 (grid, folds)
