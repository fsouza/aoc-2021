open StdLabels
open MoreLabels

module Pos = struct
  type t = int * int * int

  let compare (x1, y1, z1) (x2, y2, z2) =
    let x = x1 - x2 in
    let y = y1 - y2 in
    let z = z1 - z2 in
    if x <> 0 then x else if y <> 0 then y else z
end

module IntMap = Map.Make (Int)
module IntSet = Set.Make (Int)
module PosSet = Set.Make (Pos)

type position = Pos.t

type scanner = {
  name : string;
  xs : int list;
  ys : int list;
  zs : int list;
  pos : position;
}

module Scanner = struct
  type t = scanner

  let compare { name = name1; _ } { name = name2; _ } =
    String.compare name1 name2
end

module ScannerMap = Map.Make (Scanner)
module ScannerSet = Set.Make (Scanner)

let parse_beacon line =
  match String.split_on_char ~sep:',' line with
  | [ x; y; z ] -> Some (int_of_string x, int_of_string y, int_of_string z)
  | _ -> None

let head_regexp = Str.regexp {|^--- scanner \([0-9]+\) ---$|}

let parse_head line =
  if Str.string_match head_regexp line 0 then
    let id = Str.matched_group 1 line in
    Some id
  else None

let parse_scanner input =
  match String.split_on_char ~sep:'\n' input with
  | [] -> None
  | head :: beacon_lines ->
      parse_head head
      |> Option.map (fun head ->
             let xs, ys, zs =
               beacon_lines
               |> List.filter_map ~f:parse_beacon
               |> List.fold_left ~init:([], [], [])
                    ~f:(fun (xs, ys, zs) (x, y, z) ->
                      (x :: xs, y :: ys, z :: zs))
             in
             { name = head; xs; ys; zs; pos = (0, 0, 0) })

let print_diffs =
  IntMap.iter ~f:(fun ~key ~data ->
      if data > 1 then Printf.printf "  %d: %d\n" key data)

let print_axis = List.iter ~f:(Printf.printf "- %d\n")
let negative = List.map ~f:(( * ) (-1))

let check_adjency { xs = xs1; ys = ys1; zs = zs1; _ }
    ({ xs = xs2; ys = ys2; zs = zs2; _ } as scanner2) =
  (* Takes two scanners and returns an option: if the second scanner is adjancent
      to the first one, returns Some <scanner>, which will be the second scanner
     with the proper x, y and z axis, along with its position, all relative to
     the position of scanner1.
  *)
  let get_diffs axis1 axis2 =
    axis1
    |> List.map ~f:(fun v1 -> axis2 |> List.map ~f:(fun v2 -> v1 - v2))
    |> List.fold_left ~init:IntMap.empty ~f:(fun freqs ->
           List.fold_left ~init:freqs ~f:(fun freqs diff ->
               let curr =
                 IntMap.find_opt diff freqs |> Option.value ~default:0
               in
               IntMap.add ~key:diff ~data:(curr + 1) freqs))
  in
  let other_axis =
    [| xs2; ys2; zs2; negative xs2; negative ys2; negative zs2 |]
  in
  let other_idx idx = if idx < 3 then idx + 3 else idx - 3 in
  let rec find_index ?f axis visited idx =
    let f = f |> Option.value ~default:(fun freq -> freq >= 12) in
    if idx = Array.length other_axis then None
    else if IntSet.mem idx visited then find_index axis visited (idx + 1)
    else
      let diffs =
        get_diffs axis other_axis.(idx)
        |> IntMap.filter ~f:(fun _ freq -> f freq)
      in
      assert (IntMap.cardinal diffs <= 1);
      match IntMap.min_binding_opt diffs with
      | None -> find_index axis visited (idx + 1)
      | Some (diff, _) -> Some (idx, diff)
  in
  let find_z ~f x_idx x_diff y_idx y_diff visited zs =
    let visited = visited |> IntSet.add y_idx |> IntSet.add (other_idx y_idx) in
    match find_index ~f zs visited 0 with
    | None -> None
    | Some (z_idx, z_diff) ->
        Some
          {
            scanner2 with
            xs = other_axis.(x_idx);
            ys = other_axis.(y_idx);
            zs = other_axis.(z_idx);
            pos = (x_diff, y_diff, z_diff);
          }
  in
  let find_y x_idx x_diff ys =
    let visited = IntSet.of_list [ x_idx; other_idx x_idx ] in
    let f = ( = ) x_diff in
    match find_index ~f ys visited 0 with
    | None -> None
    | Some (y_idx, y_diff) -> (
        let find_z = find_z ~f x_idx x_diff y_idx y_diff visited in
        match find_z zs1 with
        | None -> find_z (negative zs1)
        | r -> r)
  in
  let find_x xs =
    match find_index xs IntSet.empty 0 with
    | None -> None
    | Some (x_idx, x_diff) -> (
        let find_y = find_y x_idx x_diff in
        match find_y ys1 with
        | None -> find_y (negative ys1)
        | r -> r)
  in
  match find_x xs1 with
  | None -> find_x (negative xs1)
  | r -> r

let print_scanners =
  List.iter ~f:(fun { name; _ } -> Printf.printf "  scanner %s\n" name)

let build_adjacency_list scanners =
  let rec find_adjacents adjs scanner scanners =
    match ScannerSet.min_elt_opt scanners with
    | None -> adjs
    | Some candidate -> (
        match check_adjency scanner candidate with
        | None ->
            find_adjacents adjs scanner (ScannerSet.remove candidate scanners)
        | Some adj ->
            find_adjacents (adj :: adjs) scanner
              (ScannerSet.remove candidate scanners))
  in
  scanners
  |> ScannerSet.fold ~init:ScannerMap.empty ~f:(fun scanner ->
         ScannerMap.add ~key:scanner
           ~data:
             (find_adjacents [] scanner (ScannerSet.remove scanner scanners)))

let zip3 =
  let open Seq in
  let rec zip3 l1 l2 l3 () =
    match (l1, l2, l3) with
    | [], [], [] -> Nil
    | hd1 :: tl1, hd2 :: tl2, hd3 :: tl3 ->
        Cons ((hd1, hd2, hd3), zip3 tl1 tl2 tl3)
    | _ -> failwith "length mismatch"
  in
  zip3

let collect_beacons g =
  let rec collect_beacons' beacons visited = function
    | [] -> beacons
    | (scanner, _) :: tl when ScannerSet.mem scanner visited ->
        collect_beacons' beacons visited tl
    | (({ xs; ys; zs; pos = x, y, z; _ } as scanner), (x_acc, y_acc, z_acc))
      :: tl ->
        let xs = List.map ~f:(( + ) x_acc) xs in
        let ys = List.map ~f:(( + ) y_acc) ys in
        let zs = List.map ~f:(( + ) z_acc) zs in
        let beacons_seq = zip3 xs ys zs in
        let beacons = PosSet.add_seq beacons_seq beacons in
        let visited = ScannerSet.add scanner visited in
        let next =
          ScannerMap.find scanner g
          |> List.filter_map ~f:(fun sc ->
                 if ScannerSet.mem sc visited then None
                 else Some (sc, (x + x_acc, y + y_acc, z + z_acc)))
        in
        collect_beacons' beacons visited (tl @ next)
  in
  let ({ pos = x, y, z; _ } as first), _ = ScannerMap.min_binding g in
  collect_beacons' PosSet.empty ScannerSet.empty [ (first, (x, y, z)) ]

let () =
  let re = Str.regexp "\n\n" in
  let scanners =
    Aoc.stdin
    |> List.of_seq
    |> String.concat ~sep:"\n"
    |> Str.split re
    |> List.filter_map ~f:parse_scanner
    |> ScannerSet.of_list
    |> build_adjacency_list
  in
  scanners |> collect_beacons |> PosSet.cardinal |> Printf.printf "%d\n"
