open StdLabels

let parse_row str =
  String.split_on_char ~sep:' ' str
  |> List.to_seq
  |> Seq.map String.trim
  |> Seq.filter (fun s -> String.length s > 0)
  |> Seq.map int_of_string
  |> Seq.map (fun v -> (v, false))
  |> Array.of_seq

let parse_board arr start_idx =
  let board = Array.init ~f:(fun _ -> Array.make 5 (0, false)) 5 in
  for i = start_idx to start_idx + 4 do
    let row = parse_row arr.(i) in
    board.(i - start_idx) <- row
  done;
  board

let parse arr =
  let drawn_numbers =
    arr.(0) |> String.split_on_char ~sep:',' |> List.map ~f:int_of_string
  in
  let length = Array.length arr in
  let rec loop acc idx =
    if idx >= length then acc
    else
      let board = parse_board arr idx in
      loop (board :: acc) (idx + 6)
  in
  (drawn_numbers, loop [] 2)

exception Break

let check_board board =
  let check_row idx =
    board.(idx) |> Array.for_all ~f:(fun (_, marked) -> marked)
  in
  let check_col idx =
    let all_marked = ref true in
    for i = 0 to 4 do
      let _, marked = board.(i).(idx) in
      if not marked then all_marked := false
    done;
    !all_marked
  in
  try
    for i = 0 to 4 do
      if check_row i then raise_notrace Break
      else if check_col i then raise_notrace Break
    done;
    false
  with Break -> true

let mark_board number board =
  try
    for i = 0 to 4 do
      for j = 0 to 4 do
        let n, _ = board.(i).(j) in
        if n = number then (
          board.(i).(j) <- (n, true);
          raise_notrace Break)
      done
    done;
    (board, false)
  with Break -> (board, check_board board)

let rec play boards = function
  | [] -> None
  | hd :: tl -> (
      let marked_boards = boards |> List.map ~f:(mark_board hd) in
      match
        List.find_map
          ~f:(fun (board, won) -> if won then Some board else None)
          marked_boards
      with
      | None -> play (List.map ~f:fst marked_boards) tl
      | Some board -> Some (board, hd))

let sum_unmarked =
  Array.fold_left ~init:0 ~f:(fun acc ->
      Array.fold_left ~init:acc ~f:(fun acc (v, marked) ->
          if marked then acc else acc + v))

let () =
  let numbers, boards = Aoc.stdin |> Array.of_seq |> parse in
  match play boards numbers with
  | None -> print_endline "no winning boards?! :O"
  | Some (board, num) -> num * sum_unmarked board |> Printf.printf "%d\n"
