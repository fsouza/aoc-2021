open StdLabels

let pow2 x = x |> float_of_int |> ( ** ) 2. |> int_of_float

let to_int = function
  | '1' -> 1
  | '0' -> 0
  | _ -> invalid_arg "invalid digit passed to `to_int`"

let to_dec =
  let rec to_dec' acc exp = function
    | [] -> acc
    | hd :: tl -> to_dec' (acc + (hd * pow2 exp)) (exp + 1) tl
  in
  to_dec' 0 0

let oxygen_generator_rating input =
  let max_col =
    match input with
    | [] -> 0
    | hd :: _ -> Array.length hd
  in
  let rec loop acc col =
    if col = max_col then acc |> List.hd |> Array.to_list |> List.rev |> to_dec
    else
      let zeroes, ones =
        List.fold_left ~init:(0, 0)
          ~f:(fun (zeroes, ones) row ->
            if row.(col) = 1 then (zeroes, ones + 1) else (zeroes + 1, ones))
          acc
      in
      if zeroes > ones then
        loop (List.filter ~f:(fun entry -> entry.(col) = 0) acc) (col + 1)
      else loop (List.filter ~f:(fun entry -> entry.(col) = 1) acc) (col + 1)
  in
  loop input 0

let co2_scrubber_rating input =
  let max_col =
    match input with
    | [] -> 0
    | hd :: _ -> Array.length hd
  in
  let rec loop acc col =
    if col = max_col || List.length acc = 1 then
      acc |> List.hd |> Array.to_list |> List.rev |> to_dec
    else
      let zeroes, ones =
        List.fold_left ~init:(0, 0)
          ~f:(fun (zeroes, ones) row ->
            if row.(col) = 1 then (zeroes, ones + 1) else (zeroes + 1, ones))
          acc
      in
      if zeroes > ones then
        loop (List.filter ~f:(fun entry -> entry.(col) = 1) acc) (col + 1)
      else loop (List.filter ~f:(fun entry -> entry.(col) = 0) acc) (col + 1)
  in
  loop input 0

let () =
  let input =
    Aoc.stdin
    |> Seq.map (fun line ->
           line |> String.to_seq |> Seq.map to_int |> Array.of_seq)
    |> List.of_seq
  in
  oxygen_generator_rating input * co2_scrubber_rating input
  |> Printf.printf "%d\n"
