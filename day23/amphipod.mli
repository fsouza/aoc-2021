type t

val folded : t list array
val to_char : t -> char
val parse : char -> t option
val compare : t -> t -> int
val room_number : t -> int
val step_cost : t -> int
