let dune_file_content day_number =
  Printf.sprintf
    {|(executable
 (name day%d)
 (libraries aoc)
 (modules Day%d))
|} day_number
    day_number

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

let write_file folder name contents =
  let file_path = Filename.concat folder name in
  let file = open_out file_path in
  output_string file contents;
  close_out file

let write_dune_file day_number folder =
  write_file folder "dune" @@ dune_file_content day_number

let write_module_file day_number folder =
  write_file folder (Printf.sprintf "day%d.ml" day_number) ""

let run_fmt () = Sys.command "dune build @fmt --auto-promote" |> ignore

let () =
  let day_number = last_day () |> day_number |> ( + ) 1 in
  let folder = Printf.sprintf "day%02d" day_number in
  let folder = Filename.concat Filename.current_dir_name folder in
  Sys.mkdir folder 0o755;
  write_dune_file day_number folder;
  write_module_file day_number folder;
  run_fmt ();
  Printf.printf "Successfully created '%s'\n" folder
