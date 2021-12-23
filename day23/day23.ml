open StdLabels

type amphipod = Amber | Bronze | Copper | Desert

let room_number = function
  | Amber -> 0
  | Bronze -> 1
  | Copper -> 2
  | Desert -> 3

let pow_10 e = Float.pow 10. (Float.of_int e) |> Int.of_float
let energy a = room_number a |> pow_10

type state = { rooms : amphipod list array; hallway : amphipod option array }

let make_state rooms = { rooms; hallway = Array.make 11 None }

let parse_amphipod = function
  | 'A' -> Some Amber
  | 'B' -> Some Bronze
  | 'C' -> Some Copper
  | 'D' -> Some Desert
  | _ -> None

let parse row =
  let candidates = [ row.[3]; row.[5]; row.[7]; row.[9] ] in
  match candidates |> List.filter_map ~f:parse_amphipod with
  | [ first; second; third; fourth ] -> Some (first, second, third, fourth)
  | _ -> None

let get_rooms s =
  let result = [| []; []; []; [] |] in
  s
  |> Seq.iter (fun (first, second, third, fourth) ->
         result.(0) <- first :: result.(0);
         result.(1) <- second :: result.(1);
         result.(2) <- third :: result.(2);
         result.(3) <- fourth :: result.(3));
  result |> Array.map ~f:List.rev

let simulate state = state

let () =
  Aoc.stdin
  |> Seq.filter_map parse
  |> get_rooms
  |> make_state
  |> simulate
  |> ignore
