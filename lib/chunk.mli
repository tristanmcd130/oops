type t = Types.chunk

val empty: unit -> t
val length: t -> int
val compile: t -> Scope.t -> Ast.t -> unit
val to_closure: t -> Value.closure
val to_string: t -> string