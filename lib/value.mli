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
and type' = {mutable traits: trait list; fields: string list; methods: (string, t) Hashtbl.t}
and trait = {mutable traits: trait list; abs_methods: string list; methods: (string, t) Hashtbl.t}

module Struct: sig
	val get_method : type' list -> string -> t option
	val impl: trait option -> type' -> (string * t) list -> unit
end

module Trait: sig
	val get_method : trait list -> string -> t option
	val impl: trait option -> trait -> (string * t) list -> unit
end

val make_type: string list -> type'
val make_trait: string list -> (string * t) list -> trait
val dot: t -> string -> t
val impl: t option -> t -> (string * t) list -> unit

val null_type: type'
val bool_type: type'
val number_type: type'
val string_type: type'
val list_type: type'
val dict_type: type'
val function_type: type'
val type_type: type'
val trait_type: type'