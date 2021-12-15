open StdLabels
open MoreLabels

module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module type S = sig
  type key
  type t

  val create : capacity:int -> unit -> t
  val mem : key:key -> t -> bool
  val upsert : key:key -> priority:int -> t -> t
  val poll : t -> (key * t) option
end

module Make (Ord : OrderedType) = struct
  module OrdMap = Map.Make (Ord)

  type key = Ord.t

  type t = {
    capacity : int;
    length : int;
    data : (key * int) option array;
    key_map : int OrdMap.t;
  }

  let create ~capacity () =
    {
      capacity;
      length = 0;
      data = Array.make capacity None;
      key_map = OrdMap.empty;
    }

  (* helper functions for manipulating the array *)
  let parent_idx pos = (pos - 1) / 2

  let ensure_capacity ({ length; capacity; data; _ } as heap) =
    if length = capacity then
      let capacity = capacity * 2 in
      {
        heap with
        capacity;
        data =
          Array.init
            ~f:(fun idx -> if idx < length then data.(idx) else None)
            capacity;
      }
    else heap

  let children_idx pos =
    let d = pos * 2 in
    (d + 1, d + 2)

  let swap i j ({ data; key_map; _ } as heap) =
    let tmp = data.(i) in
    data.(i) <- data.(j);
    data.(j) <- tmp;
    let key_map =
      [ i; j ]
      |> List.filter_map ~f:(fun idx ->
             data.(idx) |> Option.map (fun (key, _) -> (key, idx)))
      |> List.to_seq
      |> (Fun.flip OrdMap.add_seq) key_map
    in
    { heap with data; key_map }

  let force_read_data arr idx =
    match arr.(idx) with
    | None -> raise Not_found
    | Some (_, data) -> data

  let force_read_key arr idx =
    match arr.(idx) with
    | None -> raise Not_found
    | Some (key, _) -> key

  let rec sink pos ({ data; length; _ } as heap) =
    let left, right = children_idx pos in
    if left >= length then heap
    else
      let left_value = force_read_data data left in
      let right_value =
        if right < length then force_read_data data right else max_int
      in
      let small_idx, small_value =
        if right_value < left_value then (right, right_value)
        else (left, left_value)
      in
      if force_read_data data pos > small_value then
        heap |> swap pos small_idx |> sink small_idx
      else heap

  let rec swin pos ({ data; _ } as heap) =
    let parent_idx = parent_idx pos in
    if pos > 0 && force_read_data data parent_idx > force_read_data data pos
    then heap |> swap pos parent_idx |> swin parent_idx
    else heap

  let fix pos heap = heap |> sink pos |> swin pos
  let mem ~key { key_map; _ } = OrdMap.mem key key_map

  let insert ~key ~priority heap =
    let ({ data; length; _ } as heap) = ensure_capacity heap in
    data.(length) <- Some (key, priority);
    { heap with data; length = length + 1 } |> swin length

  let remove ~pos ({ length; _ } as heap) =
    let heap = swap pos (length - 1) heap in
    { heap with length = length - 1 } |> fix pos

  let upsert ~key ~priority ({ key_map; _ } as heap) =
    match OrdMap.find_opt key key_map with
    | None -> insert ~key ~priority heap
    | Some pos -> heap |> remove ~pos |> insert ~key ~priority

  let poll ({ data; length; _ } as heap) =
    if length = 0 then None
    else
      let v = force_read_key data 0 in
      let heap = swap 0 (length - 1) heap in
      Some (v, { heap with length = length - 1 } |> sink 0)
end
