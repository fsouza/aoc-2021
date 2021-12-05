open StdLabels
open MoreLabels

type point = int * int

module PointMap = Map.Make (struct
  type t = point

  let compare (x1, y1) (x2, y2) =
    let x = Int.compare x1 x2 in
    if x = 0 then Int.compare y1 y2 else x
end)

type range = point * point

let line_regexp = Str.regexp " -> "

let parse_range str =
  match String.split_on_char ~sep:',' str with
  | [ x; y ] -> Some (int_of_string x, int_of_string y)
  | _ -> None

let parse str =
  match Str.split line_regexp str with
  | [ orig; dst ] -> (
      match (parse_range orig, parse_range dst) with
      | Some orig, Some dst -> Some (orig, dst)
      | _ -> None)
  | _ -> None

let gen_simple_interval start end_ =
  let op = if start < end_ then ( + ) else ( - ) in
  let cmp = if start < end_ then ( > ) else ( < ) in
  let rec seq x () =
    if cmp x end_ then Seq.Nil else Seq.Cons (x, seq (op x 1))
  in
  seq start

let rec zip seq1 seq2 () =
  let open Seq in
  match (seq1 (), seq2 ()) with
  | Cons (v1, next_seq1), Cons (v2, next_seq2) ->
      Cons ((v1, v2), zip next_seq1 next_seq2)
  | _ -> Nil

let gen_diag_interval (x1, y1) (x2, y2) =
  let x_seq = gen_simple_interval x1 x2 in
  let y_seq = gen_simple_interval y1 y2 in
  zip x_seq y_seq

let points ((x1, y1), (x2, y2)) =
  if x1 = x2 then gen_simple_interval y1 y2 |> Seq.map (fun y -> (x1, y))
  else if y1 = y2 then gen_simple_interval x1 x2 |> Seq.map (fun x -> (x, y1))
  else gen_diag_interval (x1, y1) (x2, y2)

let process state range =
  range
  |> points
  |> Seq.fold_left
       (fun state point ->
         let curr = PointMap.find_opt point state |> Option.value ~default:0 in
         PointMap.add ~key:point ~data:(curr + 1) state)
       state

let () =
  Aoc.stdin
  |> Seq.filter_map parse
  |> Seq.fold_left process PointMap.empty
  |> PointMap.to_seq
  |> Seq.filter_map (fun (_, count) -> if count > 1 then Some count else None)
  |> Seq.fold_left (fun acc _ -> acc + 1) 0
  |> Printf.printf "%d\n"
