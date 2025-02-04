type t =
| EBlock of t list
| ENull
| EBool of bool
| ENumber of float
| EString of string
| EList of t list
| EFun of string list * t
| EVar of string
| EDot of t * string
| ECall of t * t list
| EIf of t * t * t
| EAssign of string * t
| EDotAssign of t * string * t
| EDef of string * string list * t
| EClass of string * t option * t option * (string * string list * t) list
| ETrait of string * string list * (string * string list * t) list