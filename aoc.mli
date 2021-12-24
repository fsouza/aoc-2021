val stdin : string Seq.t
val take : int -> 'a Seq.t -> 'a Seq.t
val zip : 'a Seq.t -> 'b Seq.t -> ('a * 'b) Seq.t
val nat : int Seq.t
val repeat : 'a -> 'a Seq.t
val tap : f:('a -> unit) -> 'a -> 'a
