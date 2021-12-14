open StdLabels
open MoreLabels
module CharMap = Map.Make (Char)

module PairMap = Map.Make (struct
  type t = char * char

  let compare (p1c1, p1c2) (p2c1, p2c2) =
    let c1 = Char.compare p1c1 p2c1 in
    if c1 != 0 then c1 else Char.compare p1c2 p2c2
end)

let parse_insertion_rule line =
  match String.split_on_char ~sep:'-' line with
  | [ pair; insertion ] ->
      Some ((pair.[0], pair.[1]), insertion.[String.length insertion - 1])
  | _ -> None

let apply_rules rules =
  let rec apply_rules' acc = function
    | [] -> List.rev acc
    | [ solo ] -> apply_rules' (solo :: acc) []
    | first :: (second :: _ as tl) -> (
        match PairMap.find_opt (first, second) rules with
        | None -> apply_rules' acc tl
        | Some c -> apply_rules' (c :: first :: acc) tl)
  in
  apply_rules' []

let rec run steps rules polymer_template =
  if steps = 0 then polymer_template
  else run (steps - 1) rules (apply_rules rules polymer_template)

let most_common_least_common template =
  template
  |> List.fold_left ~init:CharMap.empty ~f:(fun map ch ->
         let curr = CharMap.find_opt ch map |> Option.value ~default:0 in
         CharMap.add ~key:ch ~data:(curr + 1) map)
  |> CharMap.fold ~init:('0', 0, '0', max_int)
       ~f:(fun
            ~key
            ~data
            ((most_common, most_common_amount, least_common, least_common_amount)
            as acc)
          ->
         (* not correct if there's only one item in the map, which doesn't happen in AoC *)
         if data > most_common_amount then
           (key, data, least_common, least_common_amount)
         else if data < least_common_amount then
           (most_common, most_common_amount, key, data)
         else acc)
  |> fun (_, most_common_amount, _, least_common_amount) ->
  (most_common_amount, least_common_amount)

let () =
  let polymer_template = read_line () |> String.to_seq |> List.of_seq in
  let rules =
    Aoc.stdin |> Seq.filter_map parse_insertion_rule |> PairMap.of_seq
  in
  run 10 rules polymer_template
  |> most_common_least_common
  |> (fun (most_common, least_common) -> most_common - least_common)
  |> Printf.printf "%d\n"
