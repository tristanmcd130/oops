type t =
| VNull
| VBool of bool
| VNumber of float
| VString of string
| VList of t list
| VDict of (t, t) Hashtbl.t
| VFunction of string list * Exp.t * t Env.t
| VPrimitive of (t list -> t)
| VObject of {class': class'; fields: (string, t) Hashtbl.t}
| VClass of class'
| VTrait of trait
and class'
and trait

val make_class: t option -> t option -> (string * t) list -> class'
val make_trait: string list -> (string * t) list -> trait
val dot: t -> string -> t
val add_methods: class' -> (string * (t list -> t)) list -> unit

val object_class: class'
val null_class: class'
val bool_class: class'
val number_class: class'
val string_class: class'
val list_class: class'
val dict_class: class'
val function_class: class'
val class_class: class'
val trait_class: class'