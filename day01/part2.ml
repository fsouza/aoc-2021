let read_input () = Aoc.stdin |> Seq.map int_of_string |> Array.of_seq

let window_increases arr =
  let length = Array.length arr in
  let increases = ref 0 in
  for i = 1 to length - 3 do
    let prev_window = arr.(i - 1) + arr.(i) + arr.(i + 1) in
    let this_window = arr.(i) + arr.(i + 1) + arr.(i + 2) in
    if this_window > prev_window then incr increases
  done;
  !increases

let () = read_input () |> window_increases |> Printf.printf "%d\n"
