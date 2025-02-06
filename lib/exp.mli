type t =
| EBlock of t list
| ENull
| EBool of bool
| ENumber of float
| EString of string
| EList of t list
| EDict of (t * t) list
| EFun of string list * t
| EVar of string
| EDot of t * string
| ECall of t * t list
| EIf of t * t * t
| ELet of (string * t) list * t
| EAssign of string * t
| EDotAssign of t * string * t
| EDef of string * string list * t
| EStruct of string * string list
| ETrait of string * string list * (string * string list * t) list
| EImpl of t option * t * (string * string list * t) list