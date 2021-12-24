open StdLabels
open MoreLabels

type var = W | X | Y | Z

let int_of_var = function
  | W -> 0
  | X -> 1
  | Y -> 2
  | Z -> 3

module Vars = Map.Make (struct
  type t = var

  let compare v1 v2 = Int.compare (int_of_var v1) (int_of_var v2)
end)

type state = { vars : int Vars.t; input : int array; input_idx : int }

let make_state input =
  let seq = [ W; X; Y; Z ] |> List.to_seq |> Seq.map (fun var -> (var, 0)) in
  { input; vars = Vars.add_seq seq Vars.empty; input_idx = 0 }

type value = Var of var | Lit of int
type bin_op = state -> var -> value -> state

let get_value vars = function
  | Lit x -> x
  | Var var -> Vars.find var vars

let get_operands vars var value =
  let lhs = Vars.find var vars in
  let rhs = get_value vars value in
  (lhs, rhs)

let make_bin_op int_op ({ vars; _ } as state) var value =
  let lhs, rhs = get_operands vars var value in
  { state with vars = Vars.add ~key:var ~data:(int_op lhs rhs) vars }

type instruction = Inp of var | Bin_op of bin_op * var * value

let parse_var = function
  | "w" -> Some W
  | "x" -> Some X
  | "y" -> Some Y
  | "z" -> Some Z
  | _ -> None

let parse_value v =
  match parse_var v with
  | None -> v |> int_of_string_opt |> Option.map (fun value -> Lit value)
  | Some v -> Some (Var v)

let make_op op var value =
  match (parse_var var, parse_value value) with
  | None, _ | _, None -> None
  | Some var, Some value ->
      let bin_op =
        match op with
        | "add" -> make_bin_op ( + )
        | "mul" -> make_bin_op ( * )
        | "div" -> make_bin_op ( / )
        | "mod" -> make_bin_op ( mod )
        | "eql" -> make_bin_op (fun x y -> if x = y then 1 else 0)
        | _ -> failwith "invalid binop"
      in
      Some (Bin_op (bin_op, var, value))

let parse line =
  match String.split_on_char ~sep:' ' line with
  | [ "inp"; var ] -> var |> parse_var |> Option.map (fun var -> Inp var)
  | [ op; var; value ] -> make_op op var value
  | _ -> None

let decr arr =
  let rec decr' idx =
    if idx = -1 then failwith "tried to decrement too far"
    else
      let v = arr.(idx) in
      if v = 1 then (
        arr.(idx) <- 9;
        decr' (idx - 1))
      else arr.(idx) <- v - 1
  in
  decr' (Array.length arr - 1)

let execute ({ input; vars; input_idx } as state) = function
  | Inp var ->
      let data = input.(input_idx) in
      {
        state with
        vars = Vars.add ~key:var ~data vars;
        input_idx = input_idx + 1;
      }
  | Bin_op (op, var, value) -> op state var value

let int_of_array =
  Array.fold_left ~init:0 ~f:(fun acc digit -> (acc * 10) + digit)

let cache_key { vars; _ } ip =
  let get_var v = Vars.find v vars in
  Printf.sprintf "x=%d,y=%d,w=%d,z=%d,ip=%d" (get_var X) (get_var Y) (get_var W)
    (get_var Z) ip

let find_highest_monad program =
  let start = Array.make 14 9 in
  let cache = Hashtbl.create 10 in
  let rec cached_execution state = function
    | [] -> state
    | (ip, _) :: _ as instructions -> (
        let key = cache_key state ip in
        match Hashtbl.find_opt cache key with
        | Some result -> result
        | None ->
            let result = execute_instructions state instructions in
            Hashtbl.add ~key ~data:result cache;
            result)
  and execute_instructions state = function
    | [] -> state
    | (_, inst) :: tl ->
        let state = execute state inst in
        cached_execution state tl
  in
  let rec find_highest_monad input =
    let state = make_state input in
    let { vars; _ } = cached_execution state program in
    let z = Vars.find Z vars in
    if z = 0 then int_of_array input
    else (
      decr input;
      find_highest_monad input)
  in
  find_highest_monad start

let () =
  Aoc.stdin
  |> Seq.filter_map parse
  |> Aoc.zip Aoc.nat
  |> List.of_seq
  |> find_highest_monad
  |> Printf.printf "%d\n"
