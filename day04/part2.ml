open StdLabels

let parse arr =
  let drawn_numbers =
    arr.(0) |> String.split_on_char ~sep:',' |> List.map ~f:int_of_string
  in
  let length = Array.length arr in
  let rec loop acc idx =
    if idx >= length then acc
    else
      let board = Board.parse arr idx in
      loop (board :: acc) (idx + 6)
  in
  (drawn_numbers, loop [] 2)

let rec play boards = function
  | [] -> None
  | hd :: tl ->
      if List.length boards = 1 then
        let board, won = Board.mark hd (List.hd boards) in
        if won then Some (board, hd) else play [ board ] tl
      else
        let marked_boards = boards |> List.map ~f:(Board.mark hd) in
        play
          (List.filter_map
             ~f:(fun (board, won) -> if won then None else Some board)
             marked_boards)
          tl

let () =
  let numbers, boards = Aoc.stdin |> Array.of_seq |> parse in
  match play boards numbers with
  | None -> print_endline "no winning boards?! :O"
  | Some (board, num) -> num * Board.sum_unmarked board |> Printf.printf "%d\n"
