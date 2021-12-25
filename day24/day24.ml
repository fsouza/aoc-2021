(* Note: I originally had a very slow brute force solution, this final
   solutions is inspired by Jocely Stericker's solution available at
   https://www.youtube.com/watch?v=KEUTNCRvXN4 *)

open StdLabels
open MoreLabels

type var = W | X | Y | Z

let int_of_var = function
  | W -> 0
  | X -> 1
  | Y -> 2
  | Z -> 3

module StringMap = Map.Make (String)

module Vars = Map.Make (struct
  type t = var

  let compare v1 v2 = Int.compare (int_of_var v1) (int_of_var v2)
end)

type state = { vars : int Vars.t; ip : int }

let cache_key { vars; ip } =
  let get_var var = Vars.find var vars in
  Printf.sprintf "w=%d;x=%d;y=%d;z=%d;ip=%d" (get_var W) (get_var X) (get_var Y)
    (get_var Z) ip

let initial_state =
  let seq = [ W; X; Y; Z ] |> List.to_seq |> Seq.map (fun var -> (var, 0)) in
  { vars = Vars.add_seq seq Vars.empty; ip = 0 }

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

let is_inp = function
  | Inp _ -> true
  | Bin_op _ -> false

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

let execute ({ vars; ip } as state) input = function
  | Inp var -> { vars = Vars.add ~key:var ~data:input vars; ip = ip + 1 }
  | Bin_op (op, var, value) ->
      let state = op state var value in
      { state with ip = ip + 1 }

type range = { start : int; delta : int; stop : int }

let range start end_ =
  let delta = if start > end_ then -1 else 1 in
  { start; delta; stop = end_ + delta }

let find ~range state program =
  let cache = Hashtbl.create 100 in
  let rec cached_find state =
    let cache_key = cache_key state in
    match Hashtbl.find_opt cache cache_key with
    | Some result -> result
    | None ->
        let result = find state range.start in
        Hashtbl.add ~key:cache_key ~data:result cache;
        result
  and find ({ ip; _ } as state) input =
    if input = range.stop then None
    else
      let inst = program.(ip) in
      let old_state = state in
      let state = execute state input inst in
      execute_until_inp old_state state input
  (* this old_state trick is just bad lol should I give up on recursion and embrace loops? :) *)
  and execute_until_inp old_state ({ ip; vars } as state) input =
    if ip = Array.length program then
      let z = Vars.find Z vars in
      if z = 0 then Some [ input ] else find old_state (input + range.delta)
    else
      let inst = program.(ip) in
      if is_inp inst then
        match cached_find state with
        | Some result -> Some (input :: result)
        | None -> find old_state (input + range.delta)
      else
        let state = execute state input inst in
        execute_until_inp old_state state input
  in
  cached_find state

let () =
  let program = Aoc.stdin |> Seq.filter_map parse |> Array.of_seq in
  find ~range:(range 9 1) initial_state program
  |> Option.iter (fun r ->
         Printf.printf "Part 1: ";
         List.iter ~f:print_int r;
         print_newline ());
  find ~range:(range 1 9) initial_state program
  |> Option.iter (fun r ->
         Printf.printf "Part 2: ";
         List.iter ~f:print_int r;
         print_newline ())
