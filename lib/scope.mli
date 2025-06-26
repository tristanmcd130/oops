type t
and level =
| Global
| Upvalue
| Local

val make: t option -> bool -> string list -> t
val parent: t -> t option
val in_module: t -> bool
val locals: t -> (string, int) Hashtbl.t
val upvalues: t -> (string, int * level) Hashtbl.t
val get_level: t -> string -> level
val find_local: t -> string -> int
val add_local: t -> string -> int
val add_upvalue: t -> string -> int
val resolve_locals: t -> Ast.t -> unit