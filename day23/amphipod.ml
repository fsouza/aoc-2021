type t = Amber | Bronze | Copper | Desert

let room_number = function
  | Amber -> 0
  | Bronze -> 1
  | Copper -> 2
  | Desert -> 3

let pow_10 e = Float.pow 10. (Float.of_int e) |> Int.of_float
let step_cost a = room_number a |> pow_10

let to_char = function
  | Amber -> 'A'
  | Bronze -> 'B'
  | Copper -> 'C'
  | Desert -> 'D'

let parse = function
  | 'A' -> Some Amber
  | 'B' -> Some Bronze
  | 'C' -> Some Copper
  | 'D' -> Some Desert
  | _ -> None

let compare a1 a2 = Int.compare (room_number a1) (room_number a2)
