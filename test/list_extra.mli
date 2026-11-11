include module type of Base.List

val to_string : f:('a -> string) -> 'a t -> string
val collate : 'a t -> size:int -> 'a t t
