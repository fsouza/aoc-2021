open StdLabels
open MoreLabels
module CharMap = Map.Make (Char)

let add_char_count_to_map ?(times = 1) ch map =
  let curr = CharMap.find_opt ch map |> Option.value ~default:0 in
  CharMap.add ~key:ch ~data:(curr + times) map

module PairMap = Map.Make (struct
  type t = char * char

  let compare (p1c1, p1c2) (p2c1, p2c2) =
    let c1 = Char.compare p1c1 p2c1 in
    if c1 != 0 then c1 else Char.compare p1c2 p2c2
end)

let add_pair_to_map ?(times = 1) ?(last = false) pair map =
  let curr_last, curr =
    PairMap.find_opt pair map |> Option.value ~default:(false, 0)
  in
  PairMap.add ~key:pair ~data:(curr_last || last, curr + times) map

type state = { pair_map : (bool * int) PairMap.t; char_map : int CharMap.t }

let initial_state = { pair_map = PairMap.empty; char_map = CharMap.empty }

let make_state input_line =
  let rec make_state' ({ pair_map; char_map } as state) = function
    | [] | [ _ ] -> state
    | [ first; second ] ->
        let pair = (first, second) in
        make_state'
          {
            pair_map = add_pair_to_map ~last:true pair pair_map;
            char_map =
              char_map
              |> add_char_count_to_map first
              |> add_char_count_to_map second;
          }
          []
    | first :: (second :: _ as tl) ->
        let pair = (first, second) in
        make_state'
          {
            pair_map = add_pair_to_map pair pair_map;
            char_map = char_map |> add_char_count_to_map first;
          }
          tl
  in
  input_line |> String.to_seq |> List.of_seq |> make_state' initial_state

let parse_insertion_rule line =
  match String.split_on_char ~sep:'-' line with
  | [ pair; insertion ] ->
      let first = pair.[0] in
      let second = pair.[1] in
      let to_insert = insertion.[String.length insertion - 1] in
      Some
        ( (first, second),
          (((first, to_insert), (to_insert, second)), (first, to_insert, second))
        )
  | _ -> None

let apply_rules rules state =
  PairMap.fold ~init:initial_state
    ~f:(fun ~key ~data:(last, times) state ->
      match PairMap.find_opt key rules with
      | None -> state
      | Some
          ((first_pair, second_pair), (first_char, inserted_char, second_char))
        ->
          let pair_map =
            state.pair_map
            |> add_pair_to_map ~times first_pair
            |> add_pair_to_map ~times ~last second_pair
          in
          let char_map =
            state.char_map
            |> add_char_count_to_map ~times first_char
            |> add_char_count_to_map ~times inserted_char
          in
          let char_map =
            if last then add_char_count_to_map second_char char_map
            else char_map
          in
          { pair_map; char_map })
    state.pair_map

let rec run steps rules state =
  if steps = 0 then state else run (steps - 1) rules (apply_rules rules state)

let most_common_least_common { char_map; _ } =
  char_map
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

let get_answer steps rules state =
  run steps rules state
  |> most_common_least_common
  |> fun (most_common, least_common) -> most_common - least_common

let part1 = get_answer 10
let part2 = get_answer 40

let () =
  let state = read_line () |> make_state in
  let rules =
    Aoc.stdin |> Seq.filter_map parse_insertion_rule |> PairMap.of_seq
  in
  Printf.printf "Part 1: %d\n" @@ part1 rules state;
  Printf.printf "Part 2: %d\n" @@ part2 rules state
