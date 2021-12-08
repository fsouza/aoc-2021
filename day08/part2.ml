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

let all_possibilities =
  all_chars
  |> CharSet.fold ~init:CharMap.empty ~f:(fun ch ->
         CharMap.add ~key:ch ~data:all_chars)

(* 7 - 1 = a
   0 & 1 & 6 & 9 = f
   1 = c | f, I know f, so now I know c
   2 & 3 & 5 = a | d | g, I know a, so I know d
   4 - 1 = b | d, I know d, so I know b
   0 & 6 & 9 = a | b | f | g, I know a, b and f, so now I know g
   if I know a, b, c, d, f and g, I know e :)
*)

(*
  6 digits -> 0 or 6 or 9
  2 digits -> 1
  3 digits -> 7
  4 digits -> 4
  5 digits -> 2 or 3 or 5
  7 digits -> 8
*)

let regroup_by_length =
  List.fold_left ~init:IntMap.empty ~f:(fun map pattern ->
      let length = String.length pattern in
      let curr = IntMap.find_opt length map |> Option.value ~default:[] in
      IntMap.add ~key:length ~data:(charset_of_string pattern :: curr) map)

let print_mapping =
  Hashtbl.iter ~f:(fun ~key ~data -> Printf.printf "%c: %c\n" key data)

let analyze patterns =
  Printf.printf "analyzing %s\n"
    (List.fold_left ~init:"" ~f:(fun acc p -> acc ^ " " ^ p) patterns);
  let result = Hashtbl.create 7 in
  let grouped = regroup_by_length patterns in
  let seven = grouped |> IntMap.find 3 |> List.hd in
  let one = grouped |> IntMap.find 2 |> List.hd in
  let four = grouped |> IntMap.find 4 |> List.hd in
  let a = CharSet.diff seven one in
  let abfg =
    grouped |> IntMap.find 6 |> List.fold_left ~init:all_chars ~f:CharSet.inter
  in
  let f = CharSet.inter abfg one in
  let c = CharSet.diff one f in
  let da =
    grouped |> IntMap.find 5 |> List.fold_left ~init:all_chars ~f:CharSet.inter
  in
  let d = CharSet.diff da a in
  let bd = CharSet.diff four one in
  let b = CharSet.diff bd d in
  let g = a |> CharSet.union b |> CharSet.union f |> CharSet.diff abfg in
  let e =
    a
    |> CharSet.union b
    |> CharSet.union c
    |> CharSet.union d
    |> CharSet.union f
    |> CharSet.union g
    |> CharSet.diff all_chars
  in
  Hashtbl.add ~key:(CharSet.min_elt a) ~data:'a' result;
  print_endline "added a";
  Hashtbl.add ~key:(CharSet.min_elt b) ~data:'b' result;
  print_endline "added b";
  Hashtbl.add ~key:(CharSet.min_elt c) ~data:'c' result;
  print_endline "added c";
  Hashtbl.add ~key:(CharSet.min_elt d) ~data:'d' result;
  print_endline "added d";
  Hashtbl.add ~key:(CharSet.min_elt e) ~data:'e' result;
  print_endline "added e";
  Hashtbl.add ~key:(CharSet.min_elt f) ~data:'f' result;
  print_endline "added f";
  Hashtbl.add ~key:(CharSet.min_elt g) ~data:'g' result;
  print_endline "added g";
  result

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
  let mapping = analyze patterns in
  print_mapping mapping;
  output
  |> List.to_seq
  |> Seq.map (fun str ->
         str
         |> String.to_seq
         |> Seq.map (Hashtbl.find mapping)
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
