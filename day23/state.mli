type t

val compare : t -> t -> int
val to_string : t -> string
val make : Amphipod.t list array -> t
val copy : t -> t
val is_finished : t -> bool
val path_cost : t -> step_cost:int -> origin:int -> target:int -> int option
val can_move_to_room : t -> Amphipod.t -> bool
val next : t -> (t * int) Seq.t
