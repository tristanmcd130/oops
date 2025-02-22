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
and type' = {name: string; mutable traits: trait list; fields: string list; methods: (string, t) Hashtbl.t}
and trait = {name: string; mutable traits: trait list; abs_methods: string list; methods: (string, t) Hashtbl.t}

val type_name: t -> string
val dot: t -> string -> t
val impl: trait option -> t -> (string * t) list -> unit

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
val printable_trait: trait