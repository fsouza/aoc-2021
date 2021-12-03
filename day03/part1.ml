open StdLabels

let pow2 x = x |> float_of_int |> ( ** ) 2. |> int_of_float

let to_dec =
  let rec to_dec' acc exp = function
    | [] -> acc
    | hd :: tl -> to_dec' (acc + (hd * pow2 exp)) (exp + 1) tl
  in
  to_dec' 0 0

let gamma_rate input =
  let max_col =
    match input with
    | [] -> 0
    | hd :: _ -> String.length hd
  in
  let rec gamma_rate' acc col =
    if col = max_col then acc
    else
      let zeroes, ones =
        List.fold_left ~init:(0, 0)
          ~f:(fun (zeroes, ones) row ->
            if row.[col] = '1' then (zeroes, ones + 1) else (zeroes + 1, ones))
          input
      in
      if zeroes > ones then gamma_rate' (0 :: acc) (col + 1)
      else gamma_rate' (1 :: acc) (col + 1)
  in
  gamma_rate' [] 0

let () =
  let gamma_rate = Aoc.stdin |> List.of_seq |> gamma_rate in
  let epsilon_rate = List.map ~f:(fun v -> if v = 0 then 1 else 0) gamma_rate in
  to_dec gamma_rate * to_dec epsilon_rate |> Printf.printf "%d\n"
