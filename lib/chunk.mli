type t = Types.chunk

val empty: unit -> t
val length: t -> int
val add_opcode: t -> Opcode.t -> int
val compile: t -> Scope.t -> Module.t -> Ast.t -> unit
val to_closure: t -> Module.t -> Value.closure
val to_string: t -> string