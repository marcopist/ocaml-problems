open Ring

type d

include Ring with type t := d

val of_int : int -> d
val to_int : d -> int
val to_list : d -> int list
