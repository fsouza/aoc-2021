open MoreLabels

module Pos = struct
  type t = int * int * int

  let compare (x1, y1, z1) (x2, y2, z2) =
    let x_cmp = Int.compare x1 x2 in
    if x_cmp <> 0 then x_cmp
    else
      let y_cmp = Int.compare y1 y2 in
      if y_cmp <> 0 then y_cmp else Int.compare z1 z2
end

module PosSet = Set.Make (Pos)

let line_re =
  Re.compile
    (Re.Pcre.re
       {|^(on|off) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)$|})

let option_bind ~f o = Option.bind o f

type action = On | Off

type instruction = {
  action : action;
  x_range : int * int;
  y_range : int * int;
  z_range : int * int;
}

let make_instruction action min_x max_x min_y max_y min_z max_z =
  {
    action;
    x_range = (int_of_string min_x, int_of_string max_x);
    y_range = (int_of_string min_y, int_of_string max_y);
    z_range = (int_of_string min_z, int_of_string max_z);
  }

let parse line =
  line
  |> Re.exec_opt line_re
  |> Option.map Re.Group.all
  |> option_bind ~f:(function
       | [| _; "on"; min_x; max_x; min_y; max_y; min_z; max_z |] ->
           Some (make_instruction On min_x max_x min_y max_y min_z max_z)
       | [| _; "off"; min_x; max_x; min_y; max_y; min_z; max_z |] ->
           Some (make_instruction Off min_x max_x min_y max_y min_z max_z)
       | _ -> None)

let positions (min_x, max_x) (min_y, max_y) (min_z, max_z) =
  let open Seq in
  let rec positions' x y z () =
    if x > max_x then Nil
    else if y > max_y then positions' (x + 1) min_y min_z ()
    else if z > max_z then positions' x (y + 1) min_z ()
    else Cons ((x, y, z), positions' x y (z + 1))
  in
  positions' min_x min_y min_z

let execute state { action; x_range; y_range; z_range } =
  let positions = positions x_range y_range z_range in
  match action with
  | On -> PosSet.add_seq positions state
  | Off -> positions |> Seq.fold_left (Fun.flip PosSet.remove) state

let () =
  Aoc.stdin
  |> Seq.filter_map parse
  |> Seq.fold_left execute PosSet.empty
  |> PosSet.cardinal
  |> Printf.printf "%d\n"
