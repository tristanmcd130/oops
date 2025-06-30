type t = Types.value
and closure = Types.closure
and type' = Types.type'
and trait = Types.trait

exception Runtime_error of t

val type_of: t -> type'
val dot: t -> string -> t
val base_trait: trait
val to_string: t -> string