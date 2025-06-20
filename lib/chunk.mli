type t = {
  mutable code: Opcode.t array;
  mutable constants: t Value.t array;
  mutable names: string array;
}

val add_opcode: t -> Opcode.t -> int
val add_constant: t -> t Value.t -> int
val length: t -> int
val compile: t -> Ast.t -> unit
val to_string: t -> string