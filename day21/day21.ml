open StdLabels

type player = { id : string; pos : int; score : int }

let player_regexp =
  Str.regexp {|Player \([0-9]+\) starting position: \([0-9]+\)$|}

let parse_player line =
  if Str.string_match player_regexp line 0 then
    let id = Str.matched_group 1 line in
    let pos = Str.matched_group 2 line |> int_of_string in
    { id; pos; score = 0 }
  else failwith "invalid input"

let non_zero_mod x y =
  let result = x mod y in
  if result = 0 then y else result

let add_position pos delta = non_zero_mod (pos + delta) 10
let next_die_pos pos = non_zero_mod (pos + 1) 100

let roll_die pos =
  let rec roll_die' acc pos n =
    if n = 0 then (acc, pos)
    else roll_die' (pos :: acc) (next_die_pos pos) (n - 1)
  in
  roll_die' [] pos 3

let play =
  let rec play' die_rolls die_pos ({ score; pos; _ } as player) other_player =
    let to_add, die_pos = roll_die die_pos in
    let to_add = to_add |> List.fold_left ~init:0 ~f:( + ) in
    let pos = add_position pos to_add in
    let score = score + pos in
    let die_rolls = die_rolls + 3 in
    let player = { player with score; pos } in
    if score >= 1000 then (player, other_player, die_rolls)
    else play' die_rolls die_pos other_player player
  in
  play' 0 1

let cache_key { score; pos; _ } { score = other_score; pos = other_pos; _ } =
  Printf.sprintf "%d|%d|%d|%d" score pos other_score other_pos

let all_possibilities =
  let open Seq in
  let rec all_possibilities first_die second_die third_die () =
    if first_die > 3 then Nil
    else if second_die > 3 then all_possibilities (first_die + 1) 1 1 ()
    else if third_die > 3 then all_possibilities first_die (second_die + 1) 1 ()
    else
      Cons
        ( first_die + second_die + third_die,
          all_possibilities first_die second_die (third_die + 1) )
  in
  all_possibilities 1 1 1

let quantum_play =
  let cache = Hashtbl.create 10 in
  let rec cached_play player other_player =
    let key = cache_key player other_player in
    match Hashtbl.find_opt cache key with
    | Some result -> result
    | None ->
        let result = play' player other_player in
        Hashtbl.add cache key result;
        result
  and play' ({ score; pos; _ } as player) other_player =
    if other_player.score >= 21 then (0, 1)
    else
      let positions =
        (* each roll should split the universe in 3, the process is:

           First roll, splits universe in 3:

             - First universe rolled 1
             - Second universe rolled 2
             - Third universe rolled 3

           Then each of those, will run again, adding to 9 universes:

             - 1, 1
             - 1, 2
             - 1, 3
             - 2, 1
             - 2, 2
             - 2, 3
             - 3, 1
             - 3, 2
             - 3, 3

           Finally, each of those 9 universes will run the die again, splitting
           into 9 total universes. *)
        all_possibilities |> Seq.map (add_position pos)
      in
      let scores = positions |> Seq.map (( + ) score) in
      Aoc.zip positions scores
      |> Seq.map (fun (pos, score) -> { player with pos; score })
      |> Seq.fold_left
           (fun (p1_wins_acc, p2_wins_acc) player ->
             let p2_wins, p1_wins = cached_play other_player player in
             (p1_wins + p1_wins_acc, p2_wins + p2_wins_acc))
           (0, 0)
  in
  cached_play

let best_quantum_player player1 player2 =
  let p1_wins, p2_wins = quantum_play player1 player2 in
  if p1_wins > p2_wins then p1_wins else p2_wins

let () =
  let player1 = read_line () |> parse_player in
  let player2 = read_line () |> parse_player in
  let _, { score; _ }, die_rolls = play player1 player2 in
  score * die_rolls |> Printf.printf "Part 1: %d\n";
  best_quantum_player player1 player2 |> Printf.printf "Part 2: %d\n"
