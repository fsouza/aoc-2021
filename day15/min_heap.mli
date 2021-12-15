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

module Make (Ord : OrderedType) : S with type key = Ord.t
