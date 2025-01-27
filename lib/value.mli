type t = {class': class'; repr: repr}
and class' = {name: string; super: class' option; meta: class'; fields: (string, t) Hashtbl.t; methods: (string, t) Hashtbl.t}
and repr =
| RNull
| RBool of bool
| RNumber of float
| RString of string
| RList of t array
| RFunction of string * string list * Exp.t * t Env.t
| RPrimitive of (t list -> t)
| RObject of (string, t) Hashtbl.t
| RClass of class'

val object_metaclass: class'
val make_null: unit -> t
val make_bool: bool -> t
val make_number: float -> t
val make_string: string -> t
val make_list: t list -> t
val make_function: string -> string list -> Exp.t -> t Env.t -> t
val make_primitive: (t list -> t) -> t
val make_class: string -> class' option -> (string * t) list -> (string * t) list -> (string * t) list -> class'
val of_class: class' -> t
val to_class: t -> class'
val dot: t -> string -> t
val to_bool: t -> bool