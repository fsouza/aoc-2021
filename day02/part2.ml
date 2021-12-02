open StdLabels

type instruction = Forward of int | Down of int | Up of int

type state = { pos : int * int; aim : int }

let parse line =
  match String.split_on_char ~sep:' ' line with
  | [ "forward"; v ] -> Some (Forward (int_of_string v))
  | [ "down"; v ] -> Some (Down (int_of_string v))
  | [ "up"; v ] -> Some (Up (int_of_string v))
  | _ -> None

let execute ({ pos = horizontal, depth; aim } as state) = function
  | Up x -> { state with aim = aim - x }
  | Down x -> { state with aim = aim + x }
  | Forward x -> { state with pos = (horizontal + x, depth + (aim * x)) }

let mult { pos = horizontal, depth; _ } = horizontal * depth

let () =
  Aoc.stdin
  |> Seq.filter_map parse
  |> Seq.fold_left execute { pos = (0, 0); aim = 0 }
  |> mult
  |> Printf.printf "%d\n"
