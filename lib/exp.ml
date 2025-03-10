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
| ECond of (t * t) list
| EMatch of t * (t * t) list
| ELet of (t * t) list * t
| ETry of t * (t * t) list
| EThrow of t
| EAssign of t * t
| EDotAssign of t * string * t
| EDef of string * string list * t
| EStruct of string * string list
| ETrait of string * string list * (string * string list * t) list
| EImpl of t option * t * (string * string list * t) list
| EModule of string * string list * t
| EImport of string