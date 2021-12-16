open StdLabels

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
  str
  |> String.fold_left ~init:0 ~f:(fun acc ch ->
         let d = if ch = '0' then 0 else 1 in
         (acc lsl 1) lor d)

type packet_type = Literal of int | Operator of int * packet list
and packet = { version : int; packet_type : packet_type }

let parse_raw ~pos ~len data = (String.sub ~pos ~len data, pos + len)

let parse_int ~pos ~len data =
  let value, pos = parse_raw ~pos ~len data in
  (int_of_bin_string value, pos)

let calc_padding length =
  let x = length / 4 in
  if x * 4 = length then 0 else ((x + 1) * 4) - length

let parse_literal ~pos data =
  let rec parse_literal' ~pos acc =
    let last_group_flag, pos = parse_int ~pos ~len:1 data in
    let digits, pos = parse_raw ~pos ~len:4 data in
    let acc = acc ^ digits in
    if last_group_flag = 0 then (Literal (int_of_bin_string acc), pos)
    else parse_literal' ~pos acc
  in
  parse_literal' ~pos ""

let rec parse_packet ?(padding = true) ~pos data =
  let starting_pos = pos in
  let version, pos = parse_int ~pos ~len:3 data in
  let type_id, pos = parse_int ~pos ~len:3 data in
  let remainder_parser =
    if type_id = 4 then parse_literal else parse_operator type_id
  in
  let packet_type, pos = remainder_parser ~pos data in
  let length = pos - starting_pos in
  let _, pos =
    if padding then parse_raw ~pos ~len:(calc_padding length) data else ("", pos)
  in
  ({ version; packet_type }, pos)

and parse_operator ~pos type_id data =
  let length_type_id, pos = parse_int ~pos ~len:1 data in
  let packets, pos =
    if length_type_id = 0 then parse_operator_bit_len ~pos data
    else parse_operator_packet_count ~pos data
  in
  (Operator (type_id, packets), pos)

and parse_operator_bit_len ~pos data =
  let bit_len, pos = parse_int ~pos ~len:15 data in
  let target_pos = pos + bit_len in
  let rec parse_operator_bit_len' ~pos acc =
    if pos = target_pos then (List.rev acc, pos)
    else
      let packet, pos = parse_packet ~padding:false ~pos data in
      parse_operator_bit_len' ~pos (packet :: acc)
  in
  parse_operator_bit_len' ~pos []

and parse_operator_packet_count ~pos data =
  let packet_count, pos = parse_int ~pos ~len:11 data in
  let rec parse_operator_packet_count' ~pos acc =
    if List.length acc = packet_count then (List.rev acc, pos)
    else
      let packet, pos = parse_packet ~padding:false ~pos data in
      parse_operator_packet_count' ~pos (packet :: acc)
  in
  parse_operator_packet_count' ~pos []

let rec sum_version { version; packet_type; _ } =
  match packet_type with
  | Literal _ -> version
  | Operator (_, packets) ->
      List.fold_left ~init:version
        ~f:(fun acc packet -> acc + sum_version packet)
        packets

let rec packet_value { packet_type; _ } =
  match packet_type with
  | Literal v -> v
  | Operator (0, packets) -> fold_packets ~init:0 ~f:( + ) packets
  | Operator (1, packets) -> fold_packets ~init:1 ~f:( * ) packets
  | Operator (2, packets) -> fold_packets ~init:max_int ~f:min packets
  | Operator (3, packets) -> fold_packets ~init:min_int ~f:max packets
  | Operator (5, [ packet1; packet2 ]) ->
      if packet_value packet1 > packet_value packet2 then 1 else 0
  | Operator (6, [ packet1; packet2 ]) ->
      if packet_value packet1 < packet_value packet2 then 1 else 0
  | Operator (7, [ packet1; packet2 ]) ->
      if packet_value packet1 = packet_value packet2 then 1 else 0
  | _ -> 0

and fold_packets ~init ~f =
  List.fold_left ~init ~f:(fun acc packet -> f acc (packet_value packet))

let parse_packet data =
  let packet, _ = parse_packet ~pos:0 data in
  packet

let () =
  let packet = read_line () |> hex_to_bin |> parse_packet in
  Printf.printf "Part 1: %d\n" @@ sum_version packet;
  Printf.printf "Part 2: %d\n" @@ packet_value packet
