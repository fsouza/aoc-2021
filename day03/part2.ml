open StdLabels

let pow2 x = x |> float_of_int |> ( ** ) 2. |> int_of_float

let to_dec =
  let rec to_dec' acc exp = function
    | [] -> acc
    | hd :: tl -> to_dec' (acc + (hd * pow2 exp)) (exp + 1) tl
  in
  to_dec' 0 0

let dominant input col =
  let zeroes, ones =
    List.fold_left ~init:(0, 0)
      ~f:(fun (zeroes, ones) row ->
        if row.(col) = 1 then (zeroes, ones + 1) else (zeroes + 1, ones))
      input
  in
  if zeroes > ones then Some 0 else if ones > zeroes then Some 1 else None

let oxygen_generator_rating input =
  let rec loop acc col =
    if List.length acc = 1 then
      acc |> List.hd |> Array.to_list |> List.rev |> to_dec
    else
      let val_to_keep = dominant acc col |> Option.value ~default:1 in
      loop
        (List.filter ~f:(fun entry -> entry.(col) = val_to_keep) acc)
        (col + 1)
  in
  loop input 0

let co2_scrubber_rating input =
  let rec loop acc col =
    if List.length acc = 1 then
      acc |> List.hd |> Array.to_list |> List.rev |> to_dec
    else
      let val_to_keep =
        dominant acc col
        |> Option.map (fun v -> if v = 0 then 1 else 0)
        |> Option.value ~default:0
      in
      loop
        (List.filter ~f:(fun entry -> entry.(col) = val_to_keep) acc)
        (col + 1)
  in
  loop input 0

let to_int = function
  | '1' -> 1
  | '0' -> 0
  | _ -> invalid_arg "invalid digit passed to `to_int`"

let () =
  let input =
    Aoc.stdin
    |> Seq.map (fun line ->
           line |> String.to_seq |> Seq.map to_int |> Array.of_seq)
    |> List.of_seq
  in
  oxygen_generator_rating input * co2_scrubber_rating input
  |> Printf.printf "%d\n"
