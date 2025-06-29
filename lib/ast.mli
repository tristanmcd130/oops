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
| Fun of string * string list * t
| Call of t * t list
| If of (t * t) list
| Struct of string * string list
| Dot of t * string
| Impl of t option * t * (string * string list * t) list
| Trait of string * string list * (string * string list * t) list
| Import of string * (string * string) list option
| Export of string list
| Throw of t

(* val to_string: t -> string *)