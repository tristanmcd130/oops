type t =
| Null
| Bool of bool
| Number of float
| String of string
| List of t list
| Dict of (t, t) Hashtbl.t
| Function of string * string list * Exp.t * t Env.t
| Primitive of (t list -> t)
| Struct of (type' * (string, t) Hashtbl.t)
| Type of type'
| Trait of trait
and type'
and trait

exception Runtime_error of t
val throw: type' -> string -> 'a

val make_type: string -> string list -> type'
val make_trait: string -> string list -> (string * t) list -> trait
val make_struct: type' -> t list -> t
val fields: type' -> string list
val type_name: t -> string
val dot: t -> string -> t
val dot_assign: t -> string -> t -> unit
val match': Exp.t -> t -> (string * t) list option
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
val base_trait: trait
val printable_trait: trait
val field_undefined_error_type: type'
val trait_not_implemented_error_type: type'
val primitive_error_type: type'