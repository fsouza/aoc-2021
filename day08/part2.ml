open StdLabels
open MoreLabels
module CharMap = Map.Make (Char)
module CharSet = Set.Make (Char)
module IntMap = Map.Make (Int)

let parse_patterns input = input |> String.trim |> String.split_on_char ~sep:' '

let parse str =
  match String.split_on_char ~sep:'|' str with
  | [ patterns; output ] -> Some (parse_patterns patterns, parse_patterns output)
  | _ -> None

let all_chars = CharSet.of_list [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g' ]
let charset_of_string str = str |> String.to_seq |> CharSet.of_seq

let regroup_by_length =
  List.fold_left ~init:IntMap.empty ~f:(fun map pattern ->
      let length = String.length pattern in
      let curr = IntMap.find_opt length map |> Option.value ~default:[] in
      IntMap.add ~key:length ~data:(charset_of_string pattern :: curr) map)

let is_singleton s = CharSet.cardinal s = 1

let resolve input =
  let sets =
    input
    |> regroup_by_length
    |> IntMap.fold ~init:[] ~f:(fun ~key:_ ~data acc ->
           List.fold_left ~init:all_chars ~f:CharSet.inter data :: acc)
    |> Array.of_list
  in
  let advance i j =
    let j = j + 1 in
    if j = Array.length sets then (i + 1, i + 2) else (i, j)
  in
  let rec loop acc i j =
    flush stdout;
    if List.length acc = 7 then acc
    else if i = Array.length sets - 1 then
      (* Note: this could loop forever on untrusted inputs, but the input from
                AoC it's fine. If we couldn't
                trust the input, I'd do something like checking that acc changed in
                this attempt *)
      loop acc 0 1
    else
      let acc_set =
        List.fold_left ~init:CharSet.empty ~f:(Fun.flip CharSet.add) acc
      in
      let s = CharSet.diff sets.(i) sets.(j) in
      let s = acc_set |> CharSet.diff s in
      let i, j = advance i j in
      if is_singleton s then
        let ch = CharSet.min_elt s in
        loop (ch :: acc) i j
      else loop acc i j
  in
  loop [] 0 1

let real_data =
  [
    "abcefg";
    "cf";
    "acdeg";
    "acdfg";
    "bcdf";
    "abdfg";
    "abdefg";
    "acf";
    "abcdefg";
    "abcdfg";
  ]
  |> resolve

let get_mapping patterns =
  let solution = resolve patterns in
  List.combine solution real_data |> List.to_seq |> CharMap.of_seq

let get_digit = function
  | "abcefg" -> Some '0'
  | "cf" -> Some '1'
  | "acdeg" -> Some '2'
  | "acdfg" -> Some '3'
  | "bcdf" -> Some '4'
  | "abdfg" -> Some '5'
  | "abdefg" -> Some '6'
  | "acf" -> Some '7'
  | "abcdefg" -> Some '8'
  | "abcdfg" -> Some '9'
  | _ -> None

let resolve (patterns, output) =
  let mapping = get_mapping patterns in
  output
  |> List.to_seq
  |> Seq.map (fun str ->
         str
         |> String.to_seq
         |> Seq.map ((Fun.flip CharMap.find) mapping)
         |> List.of_seq
         |> List.sort ~cmp:Char.compare
         |> List.to_seq
         |> String.of_seq)
  |> Seq.filter_map get_digit
  |> String.of_seq
  |> int_of_string

let () =
  Aoc.stdin
  |> Seq.filter_map parse
  |> Seq.map resolve
  |> Seq.fold_left ( + ) 0
  |> Printf.printf "%d\n"
