type t =
| Block of t list
| Null
| Bool of bool
| Number of float
| String of string
| List of t list
| Map of (t * t) list

val to_string: t -> string