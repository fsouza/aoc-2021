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
    else roll_die' (acc + pos) (next_die_pos pos) (n - 1)
  in
  roll_die' 0 pos 3

let play =
  let rec play' die_rolls die_pos ({ score; pos; _ } as player) other_player =
    let to_add, die_pos = roll_die die_pos in
    let pos = add_position pos to_add in
    let score = score + pos in
    let die_rolls = die_rolls + 3 in
    let player = { player with score; pos } in
    if score >= 1000 then (player, other_player, die_rolls)
    else play' die_rolls die_pos other_player player
  in
  play' 0 1

let () =
  let player1 = read_line () |> parse_player in
  let player2 = read_line () |> parse_player in
  let _, { score; _ }, die_rolls = play player1 player2 in
  score * die_rolls |> Printf.printf "%d\n"
