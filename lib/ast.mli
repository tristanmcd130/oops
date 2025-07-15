type t =
| Block of t list
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
| Struct of string * (string * t option) list
| Dot of t * string
| DotAssign of t * string * t
| Impl of t option * t * (string * string list * t) list
| Trait of string * string list * (string * string list * t) list
| Import of string * (string option, (string * string) list) Either.t
| Export of string list
| Throw of t
| Try of t * string * t
| Match of t * (t * t) list