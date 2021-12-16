let hex_digit_to_bin = function
  | '0' -> Some "0000"
  | '1' -> Some "0001"
  | '2' -> Some "0010"
  | '3' -> Some "0011"
  | '4' -> Some "0100"
  | '5' -> Some "0101"
  | '6' -> Some "0110"
  | '7' -> Some "0111"
  | '8' -> Some "1000"
  | '9' -> Some "1001"
  | 'A' -> Some "1010"
  | 'B' -> Some "1011"
  | 'C' -> Some "1100"
  | 'D' -> Some "1101"
  | 'E' -> Some "1110"
  | 'F' -> Some "1111"
  | _ -> None

let hex_to_bin str =
  str
  |> String.to_seq
  |> Seq.filter_map hex_digit_to_bin
  |> Seq.fold_left ( ^ ) ""

let int_of_bin_string str =
  let length = String.length str in
  str |> String.to_seqi

type packet = { id : int }
