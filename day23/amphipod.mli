type t

val to_string : t -> string
val parse : char -> t option
val compare : t -> t -> int
val room_number : t -> int
val step_cost : t -> int
