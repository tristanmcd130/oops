type t

val make: Opcode.t array -> t Value.t array -> string array -> t
val empty: unit -> t
val get_opcode: t -> int -> Opcode.t
val get_constant: t -> int -> t Value.t
val get_name: t -> int -> string
val length: t -> int
val compile: t -> Scope.t -> Ast.t -> unit
val to_closure: t -> t Value.closure
val to_string: t -> string