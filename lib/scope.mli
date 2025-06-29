type t = private {
  parent: t option;
  in_module: bool;
  locals: (string, int) Hashtbl.t;
  upvalues: (string, int * level) Hashtbl.t;
}
and level =
| Global
| Upvalue
| Local

val make: t option -> bool -> string list -> t
val get_level: t -> string -> level
val find_local: t -> string -> int
val add_local: t -> string -> int
val add_upvalue: t -> string -> int
val resolve_locals: t -> Ast.t -> unit