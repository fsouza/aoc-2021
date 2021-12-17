open StdLabels

type target_area = { x : int * int; y : int * int }
type probe = { pos : int * int; velocity : int * int }

let parse_coords coords =
  let parse_range range =
    match String.split_on_char ~sep:'.' range with
    | [ start; end_ ] ->
        Some
          ( int_of_string start,
            end_
            |> String.sub ~pos:1 ~len:(String.length end_ - 1)
            |> int_of_string )
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
  if x < x_lower || y < y_lower then `Before
  else if x > x_upper || y > y_upper then `After
  else `Inside

let step { pos = x, y; velocity = x_velocity, y_velocity } =
  {
    pos = (x + x_velocity, y + y_velocity);
    velocity = (next_x_velocity x_velocity, y_velocity - 1);
  }
