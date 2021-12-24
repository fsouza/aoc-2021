(* Idea for me to explore later: run a shortest path (using Dijkstra, can't
   think of a good heuristic function for A* ) on the dynamic graph of
   possible states. *)

open StdLabels
open MoreLabels
module IntSet = Set.Make (Int)

type amphipod = Amber | Bronze | Copper | Desert

let room_number = function
  | Amber -> 0
  | Bronze -> 1
  | Copper -> 2
  | Desert -> 3

let pow_10 e = Float.pow 10. (Float.of_int e) |> Int.of_float
let step_cost a = room_number a |> pow_10

type state = { rooms : amphipod list array; hallway : amphipod option array }

let door_hallway_positions = [| 2; 4; 6; 8 |]
let parking_hallway_positions = [ 0; 1; 3; 5; 7; 9; 10 ]
let make_state rooms = { rooms; hallway = Array.make 11 None }

let copy_state { rooms; hallway } =
  { rooms = Array.copy rooms; hallway = Array.copy hallway }

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

(* origin is not included in the output *)
let all_idx_between origin target =
  let step, cmp = if origin > target then (-1, ( < )) else (1, ( > )) in
  let rec move acc idx =
    if cmp idx target then acc else move (idx :: acc) (idx + step)
  in
  move [] origin

(* calculates the cost of going from origin to target given the current state.
   Returns `None` if the path is currently blocked. *)
let path_cost { hallway; _ } step_cost origin target =
  let free_hallway_positions =
    parking_hallway_positions
    |> List.filter ~f:(fun idx -> Option.is_none hallway.(idx))
    |> IntSet.of_list
  in
  let steps = all_idx_between origin target in
  if List.for_all ~f:(fun idx -> IntSet.mem idx free_hallway_positions) steps
  then Some (List.length steps * step_cost)
  else None

let can_take_amphipod { rooms; _ } number =
  match rooms.(number) with
  | [] -> true
  | [ amphipod ] when room_number amphipod = number -> true
  | _ -> false

(* calculates the possible states from the current state, by moving one
   amphipod *)
let possible_states ({ hallway; rooms } as state) =
  let occupied_hallway_positions =
    parking_hallway_positions
    |> List.filter_map ~f:(fun idx ->
           hallway.(idx) |> Option.map (fun a -> (idx, a)))
  in
  let open Seq in
  let rec gen_moves_out_of_hallway positions () =
    match positions with
    | [] -> Nil
    | (idx, amphipod) :: tl -> (
        let room_number = room_number amphipod in
        let room_length = List.length rooms.(room_number) in
        if not @@ can_take_amphipod state room_number then
          gen_moves_out_of_hallway tl ()
        else
          let door = door_hallway_positions.(room_number) in
          let step_cost = step_cost amphipod in
          let extra_steps = 2 - room_length in
          match path_cost state step_cost idx door with
          | None -> gen_moves_out_of_hallway tl ()
          | Some cost ->
              let cost = cost + (extra_steps * step_cost) in
              let new_state = copy_state state in
              let room = new_state.rooms.(room_number) in
              new_state.rooms.(room_number) <- amphipod :: room;
              new_state.hallway.(idx) <- None;
              Cons ((cost, new_state), gen_moves_out_of_hallway tl))
  in
  let rec gen_moves_out_of_rooms room_idx () =
    if room_idx = Array.length rooms then Nil else failwith "TODO"
  in
  Seq.append
    (gen_moves_out_of_hallway occupied_hallway_positions)
    (gen_moves_out_of_rooms 0)

let simulate state = state

let () =
  Aoc.stdin
  |> Seq.filter_map parse
  |> get_rooms
  |> make_state
  |> simulate
  |> ignore
