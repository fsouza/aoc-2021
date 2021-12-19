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

(* [[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]],[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]] *)
let rec reduce (e1, e2) =
  let rec step depth = function
    | Literal v when v > 9 ->
        let left = v / 2 in
        let right = float_of_int v /. 2. |> ceil |> int_of_float in
        (Pair (Literal left, Literal right), Nop, true)
    | Literal _ as e -> (e, Nop, false)
    | Pair (Literal x, Literal y) when depth = 4 ->
        (Literal 0, Explode (x, y), true)
    | Pair (e1, e2) -> (
        match step (depth + 1) e1 with
        | left, Explode (carry, v), true ->
            (Pair (left, add_literal_from_left v e2), Explode (carry, 0), true)
        | left, Nop, true -> (Pair (left, e2), Nop, true)
        | left, _, false -> (
            match step (depth + 1) e2 with
            | right, Explode (v, carry), true ->
                ( Pair (add_literal_from_right v left, right),
                  Explode (0, carry),
                  true )
            | right, Nop, true -> (Pair (left, right), Nop, true)
            | right, op, false -> (Pair (left, right), op, false)))
  in
  match step 0 (Pair (e1, e2)) with
  | (Literal _ as e), _, _ -> e
  | Pair (e1, e2), _, true -> reduce (e1, e2)
  | e, _, false -> e

let rec magnitude = function
  | Literal v -> v
  | Pair (e1, e2) -> (3 * magnitude e1) + (2 * magnitude e2)

let add pair1 pair2 = reduce (pair1, pair2)

let sum_elements = function
  | [] -> failwith "can't sum empty list of pairs"
  | [ solo ] -> solo
  | first :: tl -> List.fold_left ~init:first ~f:(fun acc p -> add acc p) tl

let%expect_test "basic reduction" =
  let input = "[[[[[9,8],1],2],3],4]" in
  input |> parse |> reduce |> string_of_elm |> print_string;
  [%expect "[[[[0,9],2],3],4]"]

let%expect_test "carry reduction" =
  let input = "[[6,[5,[4,[3,2]]]],1]" in
  input |> parse |> reduce |> string_of_elm |> print_string;
  [%expect "[[6,[5,[7,0]]],3]"]

let%expect_test "multi-step reduction" =
  let input = "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]" in
  input |> parse |> reduce |> string_of_elm |> print_string;
  [%expect "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"]

let%expect_test "reduce and split" =
  let input = "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]" in
  input |> parse |> reduce |> string_of_elm |> print_string;
  [%expect "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"]

let%expect_test "already reduced left" =
  let input = "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]" in
  input |> parse |> reduce |> string_of_elm |> print_string;
  [%expect "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]"]

let%expect_test "already reduced right" =
  let input = "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]" in
  input |> parse |> reduce |> string_of_elm |> print_string;
  [%expect "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"]

let%expect_test "complex reduction" =
  let input =
    "[[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]],[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]]"
  in
  input |> parse |> reduce |> string_of_elm |> print_string;
  [%expect "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"]

let%expect_test "chained sum" =
  [ "[1,1]"; "[2,2]"; "[3,3]"; "[4,4]" ]
  |> List.map ~f:parse
  |> List.map ~f:reduce
  |> sum_elements
  |> string_of_elm
  |> print_string;
  [%expect "[[[[1,1],[2,2]],[3,3]],[4,4]]"]

let%expect_test "chained sum with reduction" =
  [ "[1,1]"; "[2,2]"; "[3,3]"; "[4,4]"; "[5,5]"; "[6,6]" ]
  |> List.map ~f:parse
  |> List.map ~f:reduce
  |> sum_elements
  |> string_of_elm
  |> print_string;
  [%expect "[[[[5,0],[7,4]],[5,5]],[6,6]]"]

let%expect_test "large chained sum with reduction" =
  [
    "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]";
    "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]";
    "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]";
    "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]";
    "[7,[5,[[3,8],[1,4]]]]";
    "[[2,[2,2]],[8,[8,1]]]";
    "[2,9]";
    "[1,[[[9,3],9],[[9,0],[0,7]]]]";
    "[[[5,[7,4]],7],1]";
    "[[[[4,2],2],6],[8,7]]";
  ]
  |> List.map ~f:parse
  |> List.map ~f:reduce
  |> sum_elements
  |> string_of_elm
  |> print_string;
  [%expect "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"]
