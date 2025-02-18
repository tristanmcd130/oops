type t =
| VNull
| VBool of bool
| VNumber of float
| VString of string
| VList of t list
| VDict of (t, t) Hashtbl.t
| VFunction of string * string list * Exp.t * t Env.t
| VPrimitive of (t list -> t)
| VStruct of {type': type'; fields: (string, t) Hashtbl.t}
| VType of type'
| VTrait of trait
and type'
and trait

val make_type: string -> string list -> t
val make_trait: string -> string list -> (string * t) list -> t
val make_struct: type' -> t list -> t
val type_name: type' -> string
val dot: t -> string -> t
val impl: t option -> type' -> (string * t) list -> unit

val null_type: type'
val bool_type: type'
val number_type: type'
val string_type: type'
val list_type: type'
val dict_type: type'
val function_type: type'
val type_type: type'
val trait_type: type'
val module_type: type'