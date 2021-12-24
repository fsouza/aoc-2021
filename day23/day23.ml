open StdLabels
open MoreLabels
module State_heap = Min_heap.Make (State)
module State_set = Set.Make (State)

let parse row =
  let candidates = [ row.[3]; row.[5]; row.[7]; row.[9] ] in
  match candidates |> List.filter_map ~f:Amphipod.parse with
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

let simulate state =
  let rec simulate' queue visited =
    match State_heap.poll_key_priority queue with
    | None -> None
    | Some (state, base_cost, queue) ->
        if State.is_finished state then Some base_cost
        else if State_set.mem state visited then simulate' queue visited
        else
          let queue =
            State.next state
            |> Seq.fold_left
                 (fun queue (state, cost) ->
                   let cost = base_cost + cost in
                   State_heap.insert ~key:state ~priority:cost queue)
                 queue
          in
          simulate' queue (State_set.add state visited)
  in
  let queue = State_heap.create ~capacity:10 () in
  let queue = State_heap.insert ~key:state ~priority:0 queue in
  simulate' queue State_set.empty

let () =
  Aoc.stdin
  |> Seq.filter_map parse
  |> get_rooms
  |> State.make
  |> simulate
  |> Option.iter (Printf.printf "%d\n")
