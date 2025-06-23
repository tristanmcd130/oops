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
| Fun of string * string list * t
| Call of t * t list
| If of (t * t) list
| Struct of string * string list
| Dot of t * string
| Impl of t option * t * (string * string list * t) list
| Trait of string * string list * (string * string list * t) list
and unary_op =
| Negate
| Not
and binary_op =
| Add
| Subtract
| Multiply
| Divide
| Modulo
| LT
| LE
| EQ
| NE
| GT
| GE
| And
| Or
| Cons

(* val to_string: t -> string *)