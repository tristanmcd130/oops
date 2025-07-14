type t = Types.value
and closure = Types.closure
and type' = Types.type'
and trait = Types.trait

exception Runtime_error of t

val type_of: t -> type'
val get_field: t -> string -> t
val set_field: t -> string -> t -> unit
val impl: trait -> t -> unit
val base_trait: trait
val bool_type: type'
val number_type: type'
val string_type: type'
val list_type: type'
val map_type: type'
val function_type: type'
val type_type: type'
val trait_type: type'
val module_type: type'
val to_string: t -> string