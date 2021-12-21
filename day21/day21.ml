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

let quantum_play =
  let rec play' ({ score; pos; id } as player) other_player =
    let positions = [ 1; 2; 3 ] |> List.map ~f:(add_position (pos * 3)) in
    let scores = positions |> List.map ~f:(( + ) score) in
    List.map2 ~f:(fun pos score -> { player with pos; score }) positions scores
    |> List.fold_left ~init:(0, 0) ~f:(fun (p1_wins_acc, p2_wins_acc) player ->
           if player.score >= 21 then
             if id = "1" then (p1_wins_acc + 1, p2_wins_acc)
             else (p1_wins_acc, p2_wins_acc + 1)
           else
             let p1_wins, p2_wins = play' other_player player in
             (p1_wins + p1_wins_acc, p2_wins + p2_wins_acc))
  in
  play'

let best_quantum_player player1 player2 =
  let p1_wins, p2_wins = quantum_play player1 player2 in
  Printf.printf "P1 wins: %d\tP2 wins: %d\n" p1_wins p2_wins;
  if p1_wins > p2_wins then p1_wins else p2_wins

let () =
  let player1 = read_line () |> parse_player in
  let player2 = read_line () |> parse_player in
  let _, { score; _ }, die_rolls = play player1 player2 in
  score * die_rolls |> Printf.printf "Part 1: %d\n";
  best_quantum_player player1 player2 |> Printf.printf "Part 2: %d\n"
