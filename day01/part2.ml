let read_input () = Aoc.stdin |> Seq.map int_of_string |> List.of_seq

let count_increases =
  let rec count_increases' acc = function
    | [] | [ _ ] | [ _; _ ] | [ _; _; _ ] -> acc
    | first :: (second :: third :: fourth :: _ as tl) ->
        let prev_window = first + second + third in
        let this_window = second + third + fourth in
        if this_window > prev_window then count_increases' (acc + 1) tl
        else count_increases' acc tl
  in
  count_increases' 0

let () = read_input () |> count_increases |> Printf.printf "%d\n"
