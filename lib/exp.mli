type t =
| EBlock of t list
| ENull
| EBool of bool
| ENumber of float
| EString of string
| EList of t list
| ELambda of string list * t
| EVar of string
| EDot of t * string
| ECall of t * t list
| EIf of (t * t) list
| EWhile of t * t
| EFor of string * t * t
| EAssign of string * t
| EFun of string * string list * t
| EClass of string * string option * t list