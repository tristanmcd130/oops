type t =
| Block of t list
| Null
| Bool of bool
| Number of float
| String of string
| List of t list
| Map of (t * t) list
| Var of string
| Assign of string * t
| Unary of unary_op * t
| Binary of t * binary_op * t
| Fun of string list * t
| Call of t * t list
and unary_op =
| Negate
and binary_op =
| Add
| Subtract
| Multiply
| Divide
| Modulo

val to_string: t -> string