(* Idea for me to explore later: run a shortest path (using Dijkstra, can't
   think of a good heuristic function for A* ) on the dynamic graph of
   possible states. *)

open StdLabels
open MoreLabels
module IntSet = Set.Make (Int)

type amphipod = Amber | Bronze | Copper | Desert

let string_of_amphipod = function
  | Amber -> "A"
  | Bronze -> "B"
  | Copper -> "C"
  | Desert -> "D"

let room_number = function
  | Amber -> 0
  | Bronze -> 1
  | Copper -> 2
  | Desert -> 3

let amphipod_eql a1 a2 = room_number a1 = room_number a2
let pow_10 e = Float.pow 10. (Float.of_int e) |> Int.of_float
let step_cost a = room_number a |> pow_10

type state = { rooms : amphipod list array; hallway : amphipod option array }

let is_finished { rooms; hallway } =
  let rec check_rooms idx =
    if idx = Array.length rooms then true
    else
      let room = rooms.(idx) in
      if List.for_all ~f:(fun amphipod -> room_number amphipod = idx) room then
        check_rooms (idx + 1)
      else false
  in
  Array.for_all ~f:Option.is_none hallway && check_rooms 0

let print_state { rooms; hallway } =
  rooms
  |> Array.to_seqi
  |> Seq.iter (fun (i, room) ->
         let room =
           room |> List.map ~f:string_of_amphipod |> String.concat ~sep:","
         in
         Printf.printf "Room: %d: %s\n" i room);
  hallway
  |> Array.to_list
  |> List.map ~f:(function
       | None -> "."
       | Some a -> string_of_amphipod a)
  |> String.concat ~sep:""
  |> Printf.printf "\nHallway: %s\n"

module State = struct
  type t = state

  let compare_amphipod a1 a2 = Int.compare (room_number a1) (room_number a2)

  let compare_hallway_pos a1 a2 =
    match (a1, a2) with
    | None, None -> 0
    | _, None -> 1
    | None, _ -> -1
    | Some a1, Some a2 -> compare_amphipod a1 a2

  let compare { rooms = rooms1; hallway = hallway1 }
      { rooms = rooms2; hallway = hallway2 } =
    let rec compare_rooms idx =
      if idx = Array.length rooms1 then 0
      else
        match (rooms1.(idx), rooms2.(idx)) with
        | [], [] -> compare_rooms (idx + 1)
        | l1, l2 when List.length l1 < List.length l2 -> -1
        | l1, l2 when List.length l1 > List.length l2 -> 1
        | l1, l2 ->
            let rec compare_amphipods l1 l2 =
              match (l1, l2) with
              | [], [] -> compare_rooms (idx + 1)
              | [], _ -> -1
              | _, [] -> 1
              | hd1 :: tl1, hd2 :: tl2 ->
                  let cmp = compare_amphipod hd1 hd2 in
                  if cmp <> 0 then cmp else compare_amphipods tl1 tl2
            in
            compare_amphipods l1 l2
    in
    let rec compare_hallway idx =
      if idx = Array.length hallway1 then compare_rooms 0
      else
        let r = compare_hallway_pos hallway1.(idx) hallway2.(idx) in
        if r <> 0 then r else compare_hallway (idx + 1)
    in
    compare_hallway 0
end

module State_heap = Min_heap.Make (State)
module State_set = Set.Make (State)

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
let gen_steps origin target =
  let step, cmp = if origin > target then (-1, ( < )) else (1, ( > )) in
  let rec move acc idx =
    if cmp idx target then acc else move (idx :: acc) (idx + step)
  in
  move [] (origin + step)

(* calculates the cost of going from origin to target given the current state.
   Returns `None` if the path is currently blocked. *)
let path_cost { hallway; _ } step_cost origin target =
  let door_positions =
    door_hallway_positions |> Array.to_seq |> IntSet.of_seq
  in
  let free_hallway_positions =
    parking_hallway_positions
    |> List.filter ~f:(fun idx -> Option.is_none hallway.(idx))
    |> IntSet.of_list
    |> IntSet.union door_positions
  in
  let steps = gen_steps origin target in
  if List.for_all ~f:(fun idx -> IntSet.mem idx free_hallway_positions) steps
  then Some (List.length steps * step_cost)
  else None

let can_move_to_room { rooms; _ } amphipod =
  let room_number = room_number amphipod in
  match rooms.(room_number) with
  | [] -> true
  | [ other_amphipod ] when amphipod_eql other_amphipod amphipod -> true
  | _ -> false

(* calculates the possible states from the current state, by moving one
   amphipod into the hallway or into a room *)
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
    | (idx, amphipod) :: tl ->
        if can_move_to_room state amphipod then (
          let room_number = room_number amphipod in
          let room_length = List.length rooms.(room_number) in
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
              Cons ((new_state, cost), gen_moves_out_of_hallway tl))
        else gen_moves_out_of_hallway tl ()
  in
  let rec gen_moves_out_of_rooms room_idx () =
    if room_idx = Array.length rooms then Nil
    else
      match rooms.(room_idx) with
      | [] -> Nil
      | l when List.for_all ~f:(fun a -> room_number a = room_idx) l -> Nil
      | amphipod :: tl ->
          let door = door_hallway_positions.(room_idx) in
          let rec generate_moves free_hallway_positions () =
            match free_hallway_positions with
            | [] -> Nil
            | hallway_pos :: free_hallway_positions -> (
                let step_cost = step_cost amphipod in
                let extra_steps = 2 - List.length rooms.(room_idx) + 1 in
                match path_cost state step_cost door hallway_pos with
                | None -> generate_moves free_hallway_positions ()
                | Some cost ->
                    let cost = cost + (extra_steps * step_cost) in
                    let new_state = copy_state state in
                    new_state.rooms.(room_idx) <- tl;
                    new_state.hallway.(hallway_pos) <- Some amphipod;
                    Cons
                      ((new_state, cost), generate_moves free_hallway_positions)
                )
          in
          let free_hallway_positions =
            parking_hallway_positions
            |> List.filter ~f:(fun idx -> Option.is_none hallway.(idx))
          in
          Cons
            ( generate_moves free_hallway_positions,
              gen_moves_out_of_rooms (room_idx + 1) )
  in
  Seq.append
    (gen_moves_out_of_hallway occupied_hallway_positions)
    (Seq.concat @@ gen_moves_out_of_rooms 0)

let simulate state =
  let rec simulate' queue visited =
    match State_heap.poll_key_priority queue with
    | None -> None
    | Some (state, base_cost, queue) ->
        if is_finished state then Some base_cost
        else if State_set.mem state visited then simulate' queue visited
        else
          let queue =
            possible_states state
            |> Seq.fold_left
                 (fun queue (state, cost) ->
                   State_heap.insert ~key:state ~priority:(base_cost + cost)
                     queue)
                 queue
          in
          simulate' queue (State_set.add state visited)
  in
  let queue = State_heap.create ~capacity:10 () in
  let queue = State_heap.upsert ~key:state ~priority:0 queue in
  simulate' queue State_set.empty

let () =
  Aoc.stdin
  |> Seq.filter_map parse
  |> get_rooms
  |> make_state
  |> simulate
  |> Option.iter (Printf.printf "%d\n")
