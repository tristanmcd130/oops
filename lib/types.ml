type value =
| Bool of bool
| Number of float
| String of string
| List of value list
| Map of (value, value) Hashtbl.t
| Closure of closure
| Cell of value ref
| Primitive of (value list -> value)
| Struct of type' * value array
| Type of type'
| Trait of trait
| Method of value * closure
| Module of module'
and closure = {
  name: string;
  module': module';
  chunk: chunk;
  num_args: int;
  num_locals: int;
  upvalues: value ref array;
}
and type' = {
  name: string;
  fields: (string, int) Hashtbl.t;
  methods: (string, value) Hashtbl.t;
  mutable traits: trait list;
}
and trait = {
  name: string;
  requires: string list;
  provides: (string, value) Hashtbl.t;
}
and module' = {
  filename: string;
  vars: (string, value) Hashtbl.t;
  mutable exports: string list;
}
and chunk = {
  mutable code: Opcode.t array;
  mutable constants: value array;
  mutable names: string array;
  mutable handlers: (int * int) list;
}