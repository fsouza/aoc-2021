open StdLabels

type pair = elm * elm
and elm = Literal of int | Pair of pair

type stack_el = Stack_literal of char | Int of int | Stack_pair of pair

let is_digit ch = ch >= '0' && ch <= '9'
let parse_digit ch = Char.code ch - Char.code '0'

let make_pair = function
  | Int y :: Int x :: Stack_literal '[' :: tl ->
      Stack_pair (Literal x, Literal y) :: tl
  | Int y :: Stack_pair p :: Stack_literal '[' :: tl ->
      Stack_pair (Pair p, Literal y) :: tl
  | Stack_pair p :: Int x :: Stack_literal '[' :: tl ->
      Stack_pair (Literal x, Pair p) :: tl
  | Stack_pair p2 :: Stack_pair p1 :: Stack_literal '[' :: tl ->
      Stack_pair (Pair p1, Pair p2) :: tl
  | _ -> failwith "malformed expression"

let get_pair_from_stack_exn = function
  | [ Stack_pair p ] -> p
  | _ ->
      failwith "malformed expression: didn't reduce expression to a single pair"

let parse input =
  let rec parse' stack idx =
    if idx = String.length input then stack
    else
      let ch = input.[idx] in
      match ch with
      | '[' -> parse' (Stack_literal ch :: stack) (idx + 1)
      | ch when is_digit ch -> parse' (Int (parse_digit ch) :: stack) (idx + 1)
      | ']' -> parse' (make_pair stack) (idx + 1)
      | _ -> parse' stack (idx + 1)
  in
  parse' [] 0 |> get_pair_from_stack_exn

let rec sexp_of_elm = function
  | Literal v -> Printf.sprintf "(Literal %d)" v
  | Pair (e1, e2) ->
      Printf.sprintf "(Pair %s, %s)" (sexp_of_elm e1) (sexp_of_elm e2)

let rec string_of_elm = function
  | Literal v -> Int.to_string v
  | Pair (e1, e2) ->
      Printf.sprintf "[%s,%s]" (string_of_elm e1) (string_of_elm e2)

type reduction = Explode of int * int | Nop

let rec add_literal_from_left v = function
  | Literal x -> Literal (x + v)
  | Pair (left, right) -> Pair (add_literal_from_left v left, right)

let rec add_literal_from_right v = function
  | Literal x -> Literal (x + v)
  | Pair (left, right) -> Pair (left, add_literal_from_right v right)

let reduce (e1, e2) =
  let rec reduce' ?(exploded = false) depth = function
    | Literal _ as e -> (e, Nop)
    | Pair (Literal x, Literal y) when exploded ->
        (Pair (Literal x, Literal y), Nop)
    | Pair (Literal x, Literal y) when depth = 4 -> (Literal 0, Explode (x, y))
    | Pair (e1, e2) -> (
        let left, op_left = reduce' ~exploded (depth + 1) e1 in
        let exploded =
          match op_left with
          | Nop -> exploded || false
          | Explode _ -> true
        in
        let right, op_right = reduce' ~exploded (depth + 1) e2 in
        match (op_left, op_right) with
        | Nop, Nop | Explode (0, 0), Nop | Nop, Explode (0, 0) ->
            (Pair (left, right), Nop)
        | Explode (carry, v), Nop ->
            (Pair (left, add_literal_from_left v right), Explode (carry, 0))
        | Nop, Explode (v, carry) ->
            (Pair (add_literal_from_right v left, right), Explode (0, carry))
        | _ -> failwith "state violation: two actions in a single reduction")
  in
  reduce' 0 (Pair (e1, e2)) |> fst

let magnitude (e1, e2) =
  let rec magnitude' = function
    | Literal v -> v
    | Pair (e1, e2) -> (3 * magnitude' e1) + (2 * magnitude' e2)
  in
  magnitude' (Pair (e1, e2))

let add pair1 pair2 = Pair (pair1, pair2)

let () =
  Aoc.stdin
  |> Seq.map parse
  |> Seq.map reduce
  |> Seq.map string_of_elm
  |> Seq.iter (Printf.printf "%s\n")
