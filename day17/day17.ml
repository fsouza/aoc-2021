open StdLabels

type target_area = { x : int * int; y : int * int }
type probe = { pos : int * int; velocity : int * int; high_y : int }

let make_probe x_vel y_vel =
  { pos = (0, 0); velocity = (x_vel, y_vel); high_y = min_int }

let parse_coords coords =
  let parse_range range =
    match String.split_on_char ~sep:'.' range with
    | [ start; _; end_ ] -> Some (int_of_string start, int_of_string end_)
    | _ -> None
  in
  match String.split_on_char ~sep:'=' coords with
  | [ _; range ] -> parse_range range
  | _ -> None

let parse_target_area line =
  let prefix = "target area: " in
  let coords =
    String.sub ~pos:(String.length prefix)
      ~len:(String.length line - String.length prefix)
      line
  in
  match String.split_on_char ~sep:',' coords with
  | [ x; y ] ->
      Option.bind (parse_coords x) (fun x ->
          Option.map (fun y -> { x; y }) (parse_coords y))
  | _ -> None

let next_x_velocity v = if v = 0 then v else if v < 0 then v + 1 else v - 1

let check_area { pos = x, y; _ } { x = x_lower, x_upper; y = y_lower, y_upper }
    =
  if x > x_upper || y < y_lower then `After
  else if x < x_lower then `Before_x
  else if y > y_upper then `Before_y
  else `Inside

let step { pos = x, y; velocity = x_velocity, y_velocity; high_y } =
  let y = y + y_velocity in
  let high_y = max y high_y in
  {
    pos = (x + x_velocity, y);
    velocity = (next_x_velocity x_velocity, y_velocity - 1);
    high_y;
  }

(* Not totally sure how to solve this with math, will do some research after trying my own solution.

   Notes, for the reader and for myself:

     There's a range for x_velocity and y_velocity, so I can loop inside that
     range and try all possibilities, then pick whichever has the highest y
     position (assuming y_velocity starts positive, the high y position is
     reached at y_velocity = 0).

     Given that the target area is defined as {x=(x_low, x_high); y=(y_low,
     y_high)}, here's my thought process on such ranges.

     X velocity:
       - lower bound: v such that v + (v - 1) + (v - 2) + (v - 3) ... + 1 =
         x_low. Anything lower than v will decay to 0 before reaching x_low and
         never get to the target area. Can we calc this without a for loop?
         Usin the sum of an AP maybe?
       - upper bound: x_high, if x velocity is higher than x_high, we'll shoot
       past it in the very first step.

     Y velocity:
       - lower bound: for part 1, it's 1 (we want highest y, we must go up).
         For part 2, since we want all possibilities, I decided to try -(abs
         y_low) and that worked.
       - upper bound: (abs y_low)? 🤔 Not sure, need something to try lol *)

let string_of_probe { pos = x, y; velocity = vel_x, vel_y; _ } =
  Printf.sprintf "{pos = (%d, %d); velocity = (%d, %d)}" x y vel_x vel_y

let rec launch target_area ({ velocity = x_vel, _; _ } as probe) =
  match check_area probe target_area with
  | `Before_x when x_vel = 0 -> None
  | `Before_x | `Before_y -> launch target_area (step probe)
  | `After -> None
  | `Inside -> Some probe

let calc_min_x_vel x_low =
  (* S = n * (a1 + an) / 2

     Given that a1 = 1, an = n and S = x_low, we get:

     x_low = n ^ 2 + n
     n ^ 2 + n - x_low = 0

     Just need to solve that equation and consider only the positive root. Let's
     use Bhaskara here.
  *)
  let c = float_of_int (-x_low) in
  let positive_root = 1. +. ((sqrt @@ (1. -. (4. *. c))) /. 2.) in
  int_of_float positive_root

let run ({ x = x_low, x_high; y = y_low, _ } as target_area) =
  let min_x_vel = calc_min_x_vel x_low in
  let max_x_vel = x_high in
  let min_y_vel = y_low in
  let max_y_vel = abs y_low in
  let next y_vel x_vel =
    let x_vel = x_vel + 1 in
    if x_vel > max_x_vel then (y_vel + 1, min_x_vel) else (y_vel, x_vel)
  in
  let rec run' acc y_vel x_vel =
    if y_vel > max_y_vel then acc
    else
      let probe = make_probe x_vel y_vel in
      let y_vel, x_vel = next y_vel x_vel in
      match launch target_area probe with
      | None -> run' acc y_vel x_vel
      | Some probe -> run' (probe :: acc) y_vel x_vel
  in
  run' [] min_y_vel min_x_vel

let part1 probes =
  probes
  |> Option.map
       (List.fold_left ~init:min_int ~f:(fun acc { high_y; _ } ->
            max acc high_y))
  |> Option.iter (Printf.printf "Part 1: %d\n")

let part2 probes =
  probes
  |> Option.map (List.fold_left ~init:0 ~f:(fun acc _ -> acc + 1))
  |> Option.iter (Printf.printf "Part 2: %d\n")

let () =
  let all_probes = read_line () |> parse_target_area |> Option.map run in
  part1 all_probes;
  part2 all_probes
