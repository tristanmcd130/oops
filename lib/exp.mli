type t =
| Block of t list
| Null
| Bool of bool
| Number of float
| String of string
| List of t list
| Dict of (t * t) list
| Fun of string list * t
| Var of string
| Dot of t * string
| Call of t * t list
| If of (t * t) list
| Match of t * (t * t) list
| Let of (t * t) list * t
| Try of t * (t * t) list
| Throw of t
| Assign of t * t
| DotAssign of t * string * t
| Def of string * string list * t
| Struct of string * string list
| Trait of string * string list * (string * string list * t) list
| Impl of t option * t * (string * string list * t) list
| Module of string * string list * t
| Import of string