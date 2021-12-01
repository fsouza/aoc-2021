let dune_file_content =
  {|(executable
 (name part1)
 (libraries aoc)
 (modules Part1))
|}

let last_day () =
  Sys.readdir Filename.current_dir_name
  |> Array.to_seq
  |> Seq.filter (String.starts_with ~prefix:"day")
  |> Seq.fold_left max "day00"

let day_number day =
  let day_length = String.length day in
  let number_length = 2 in
  let start_pos = day_length - number_length in
  String.sub day start_pos number_length |> int_of_string

let folder_name () =
  last_day () |> day_number |> ( + ) 1 |> Printf.sprintf "day%02d"

let write_file folder name contents =
  let file_path = Filename.concat folder name in
  let file = open_out file_path in
  output_string file contents;
  close_out file

let write_dune_file folder = write_file folder "dune" dune_file_content

let write_part1_module folder = write_file folder "part1.ml" ""

let run_fmt () = Sys.command "dune build @fmt --auto-promote" |> ignore

let () =
  let folder = folder_name () in
  let folder = Filename.concat Filename.current_dir_name folder in
  Sys.mkdir folder 0o755;
  write_dune_file folder;
  write_part1_module folder;
  run_fmt ();
  Printf.printf "Successfully created '%s'\n" folder
