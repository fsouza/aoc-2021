open StdLabels
open MoreLabels
module IntSet = Set.Make (Int)

type t = {
  rooms : Amphipod.t list array;
  hallway : Amphipod.t option array;
  room_capacity : int;
}

let make rooms =
  { rooms; hallway = Array.make 11 None; room_capacity = List.length rooms.(0) }

let copy { rooms; hallway; room_capacity } =
  { rooms = Array.copy rooms; hallway = Array.copy hallway; room_capacity }

let door_hallway_positions = [| 2; 4; 6; 8 |]
let parking_hallway_positions = [ 0; 1; 3; 5; 7; 9; 10 ]

let to_string { rooms; hallway; room_capacity } =
  let first_row = "#############" in
  let hallway_str =
    hallway
    |> Array.to_list
    |> List.map ~f:(function
         | None -> '.'
         | Some a -> Amphipod.to_char a)
    |> List.to_seq
    |> String.of_seq
    |> Printf.sprintf "#%s#"
  in
  let room_rows =
    [ "###.#.#.#.###" |> String.to_seq |> Array.of_seq ]
    @ List.init ~len:(room_capacity - 1) ~f:(fun _ ->
          "  #.#.#.#.#  " |> String.to_seq |> Array.of_seq)
    |> Array.of_list
  in
  let last_row = "  #########  " in
  rooms
  |> Array.to_seqi
  |> Seq.iter (fun (idx, room) ->
         let rec chars acc = function
           | [] ->
               let acc = List.rev acc in
               let dots = room_capacity - List.length acc in
               List.init ~len:dots ~f:(Fun.const '.') @ acc
           | a :: tl -> chars (Amphipod.to_char a :: acc) tl
         in
         let pos = door_hallway_positions.(idx) + 1 in
         room
         |> chars []
         |> List.iteri ~f:(fun row ch -> room_rows.(row).(pos) <- ch));
  let room_rows =
    room_rows
    |> Array.to_seq
    |> Seq.map Array.to_seq
    |> Seq.map String.of_seq
    |> List.of_seq
  in
  [ [ first_row ]; [ hallway_str ]; room_rows; [ last_row ] ]
  |> List.flatten
  |> String.concat ~sep:"\n"

let compare_hallway_pos a1 a2 =
  match (a1, a2) with
  | None, None -> 0
  | _, None -> 1
  | None, _ -> -1
  | Some a1, Some a2 -> Amphipod.compare a1 a2

let compare { rooms = rooms1; hallway = hallway1; _ }
    { rooms = rooms2; hallway = hallway2; _ } =
  let rec compare_rooms idx =
    if idx = Array.length rooms1 then 0
    else
      match (rooms1.(idx), rooms2.(idx)) with
      | [], [] -> compare_rooms (idx + 1)
      | l1, l2 when List.length l1 < List.length l2 -> -1
      | l1, l2 when List.length l1 > List.length l2 -> 1
      | room1, room2 ->
          let rec compare_amphipods room1 room2 =
            match (room1, room2) with
            | [], [] -> compare_rooms (idx + 1)
            | [], _ -> -1
            | _, [] -> 1
            | hd1 :: tl1, hd2 :: tl2 ->
                let cmp = Amphipod.compare hd1 hd2 in
                if cmp <> 0 then cmp else compare_amphipods tl1 tl2
          in
          compare_amphipods room1 room2
  in
  let rec compare_hallway idx =
    if idx = Array.length hallway1 then compare_rooms 0
    else
      let r = compare_hallway_pos hallway1.(idx) hallway2.(idx) in
      if r <> 0 then r else compare_hallway (idx + 1)
  in
  compare_hallway 0

let is_finished { rooms; hallway; _ } =
  let rec check_rooms idx =
    if idx = Array.length rooms then true
    else
      let room = rooms.(idx) in
      if
        List.for_all
          ~f:(fun amphipod -> Amphipod.room_number amphipod = idx)
          room
      then check_rooms (idx + 1)
      else false
  in
  Array.for_all ~f:Option.is_none hallway && check_rooms 0

(* origin is not included in the output *)
let gen_steps origin target =
  let step, cmp = if origin > target then (-1, ( < )) else (1, ( > )) in
  let rec move acc idx =
    if cmp idx target then acc else move (idx :: acc) (idx + step)
  in
  move [] (origin + step)

(* calculates the cost of going from origin to target given the current state.
   Returns `None` if the path is currently blocked. *)
let path_cost { hallway; _ } ~step_cost ~origin ~target =
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

let can_move_to_room { rooms; room_capacity; _ } amphipod =
  let room_number = Amphipod.room_number amphipod in
  let room = rooms.(room_number) in
  List.length room < room_capacity
  && List.for_all ~f:(fun a -> Amphipod.compare amphipod a = 0) room

(* calculates the possible states from the current state, by moving one
   amphipod into the hallway or into a room *)
let next ({ hallway; rooms; room_capacity } as state) =
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
          let room_number = Amphipod.room_number amphipod in
          let room_length = List.length rooms.(room_number) in
          let door = door_hallway_positions.(room_number) in
          let step_cost = Amphipod.step_cost amphipod in
          let extra_steps = room_capacity - room_length in
          match path_cost state ~step_cost ~origin:idx ~target:door with
          | None -> gen_moves_out_of_hallway tl ()
          | Some cost ->
              let cost = cost + (extra_steps * step_cost) in
              let new_state = copy state in
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
      | [] -> gen_moves_out_of_rooms (room_idx + 1) ()
      | l when List.for_all ~f:(fun a -> Amphipod.room_number a = room_idx) l ->
          gen_moves_out_of_rooms (room_idx + 1) ()
      | amphipod :: tl ->
          let door = door_hallway_positions.(room_idx) in
          let rec generate_moves free_hallway_positions () =
            match free_hallway_positions with
            | [] -> Nil
            | hallway_pos :: free_hallway_positions -> (
                let step_cost = Amphipod.step_cost amphipod in
                let extra_steps =
                  room_capacity - List.length rooms.(room_idx) + 1
                in
                match
                  path_cost state ~step_cost ~origin:door ~target:hallway_pos
                with
                | None -> generate_moves free_hallway_positions ()
                | Some cost ->
                    let cost = cost + (extra_steps * step_cost) in
                    let new_state = copy state in
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
