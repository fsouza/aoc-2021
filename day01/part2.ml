let read_input () = Aoc.stdin |> Seq.map int_of_string |> Array.of_seq

let window_increases arr =
  let window_size = 3 in
  let length = Array.length arr in
  let rec window_increases' acc i =
    if i + window_size > length then acc
    else
      let prev_window = arr.(i - 1) + arr.(i) + arr.(i + 1) in
      let this_window = arr.(i) + arr.(i + 1) + arr.(i + 2) in
      if this_window > prev_window then window_increases' (acc + 1) (i + 1)
      else window_increases' acc (i + 1)
  in
  window_increases' 0 1

let () = read_input () |> window_increases |> Printf.printf "%d\n"
