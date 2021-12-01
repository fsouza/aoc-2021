let rec stdin () =
  let open Seq in
  try
    let line = read_line () in
    Cons (line, stdin)
  with End_of_file -> Nil
