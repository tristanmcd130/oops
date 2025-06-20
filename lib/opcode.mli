type t =
| GetConstant of int
| MakeList of int
| MakeMap of int

val to_string: t -> string