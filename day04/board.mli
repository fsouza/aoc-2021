type t

val parse : string array -> int -> t
val mark : int -> t -> t * bool
val sum_unmarked : t -> int
