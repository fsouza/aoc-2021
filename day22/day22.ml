(* The solution below is the brute-force, which I believe is correct, but won't
   really run on the larger sample input or the real input: it takes too long
   and uses too much memory.

   An alternative would be to represent cuboids in the plane as ranges, and then
   split them into separate cuboids as needed. Let's use a 2d space to
   understand what we need to do.

   Assuming 2d and the following instructions:

     1) on x=1..3,y=1..3
     2) on x=2..4,y=2..4
     3) off x=0..2,y=0..2
     4) on x=1..1,y=1..1

   After executing the first instrution, the plane would look something like:

     .......
     .###...
     .###...
     .###...
     .......
     .......
     .......

   Here we have one rectangle, which would be represented as a pair: the
   top-left point, and the bottom-right one, so ((1, 1), (3,3)).

   After the second instruction, the plane would become something like:

     .......
     .###...
     .####..
     .####..
     ..###..
     .......
     .......

   In that case, we could have 3 rectangles:

     - ((1,1), (3,1))
     - ((1,2), (4,3))
     - ((2,4), (4,4))

   (other representations are just possible, the specific splitting logic would
   be defined in the algorithm).

   Note: coordinates are using (x, y), with (0, 0) being the top-left. Actual
   implementation would use 3 coordinates.

   This is too annoying/tedious, so I'm not going for it right now, I may revisit
   it in the future though :)

   I think there may be an even simpler approach working solely on ranges: each instruction would cause an operation on all known ranges, where the outcomes could be:

     - for `off`: new ranges that exclude the positions that are now off
     - for `on`: a new range that merges the two ranges if they intersect
     - others? *)

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
