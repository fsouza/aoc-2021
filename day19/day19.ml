open StdLabels

type beacon = int * int * int
type scanner = { name : string; beacons : beacon list }

let string_of_beacon (x, y, z) = Printf.sprintf "%d,%d,%d" x y z

let string_of_scanner { name; beacons } =
  let lines = List.map ~f:string_of_beacon beacons in
  let lines = Printf.sprintf "--- scanner %s ---" name :: lines in
  String.concat ~sep:"\n" lines

let parse_beacon line =
  match String.split_on_char ~sep:',' line with
  | [ x; y; z ] -> Some (int_of_string x, int_of_string y, int_of_string z)
  | _ -> None

let head_regexp = Str.regexp {|^--- scanner \([0-9]+\) ---$|}

let parse_head line =
  if Str.string_match head_regexp line 0 then
    let id = Str.matched_group 1 line in
    Some id
  else None

let parse_scanner input =
  match String.split_on_char ~sep:'\n' input with
  | [] -> None
  | head :: beacons ->
      Printf.printf "parsing %s\n" head;
      parse_head head
      |> Option.map (fun head ->
             Printf.printf "parsed %s\n" head;
             { name = head; beacons = List.filter_map ~f:parse_beacon beacons })

let () =
  let re = Str.regexp "\n\n" in
  Aoc.stdin
  |> List.of_seq
  |> String.concat ~sep:"\n"
  |> Str.split re
  |> List.filter_map ~f:parse_scanner
  |> List.map ~f:string_of_scanner
  |> List.iter ~f:(Printf.printf "%s\n\n")
