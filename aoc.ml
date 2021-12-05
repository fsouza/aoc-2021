let rec stdin () =
  let open Seq in
  try
    let line = read_line () in
    Cons (line, stdin)
  with End_of_file -> Nil

let rec take n seq () =
  let open Seq in
  if n = 0 then Nil
  else
    match seq () with
    | Nil -> Nil
    | Cons (v, seq) -> Cons (v, take (n - 1) seq)

let rec zip seq1 seq2 () =
  let open Seq in
  match (seq1 (), seq2 ()) with
  | Cons (v1, seq1), Cons (v2, seq2) -> Cons ((v1, v2), zip seq1 seq2)
  | _ -> Nil
