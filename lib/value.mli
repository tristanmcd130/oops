type t
type class'
type repr

val make_null: unit -> t
val make_bool: bool -> t
val make_number: float -> t
val make_string: string -> t
val make_list: t list -> t
val make_function: string -> string list -> Exp.t -> t Env.t -> t
val make_primitive: (t list -> t) -> t
val dot: t -> string -> t
val call: t -> t list -> (Exp.t -> t Env.t -> t) -> t