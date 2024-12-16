type t =
| ABlock of t list
| ANull
| ABool of bool
| ANumber of float
| AString of string
| AList of t list
| ADict of (t * t) list
| ALambda of string list * t
| AVar of string
| ACall of t * t list
| ADot of t * string
| ASuper of string * t list
| AAssign of string * t
| ADotAssign of t * string * t
| ADef of string * string list * t
| AClass of string * string option * string list * (string * string list * t) list (* name, superclass, traits, methods *)
| ATrait of string * string list * (string * string list * t) list (* name, abstract methods, methods *)
| AIf of (t * t) list
| ALet of t list * t
| AMatch of t * (t * t) list
| AImport of string