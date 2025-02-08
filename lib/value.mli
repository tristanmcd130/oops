type t =
| VNull
| VBool of bool
| VNumber of float
| VString of string
| VList of t list
| VDict of (t, t) Hashtbl.t
| VFunction of string list * Exp.t * t Env.t
| VPrimitive of (t list -> t)
| VStruct of {type': type'; fields: (string, t) Hashtbl.t}
| VType of type'
| VTrait of trait
and type'
and trait

val make_type: string list -> type'
val make_trait: string list -> (string * t) list -> trait
val dot: t -> string -> t
val impl: t option -> t ->

val null_type: type'
val bool_type: type'
val number_type: type'
val string_type: type'
val list_type: type'
val dict_type: type'
val function_type: type'
val type_type: type'
val trait_type: type'