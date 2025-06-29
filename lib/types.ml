type value =
| Null
| Bool of bool
| Number of float
| String of string
| List of value list
| Map of (value, value) Hashtbl.t
| Closure of closure
| Cell of value ref
| Primitive of (value list -> value)
| Struct of typ * value array
| Type of typ
| Trait of trait
| Method of value * closure
and closure = {
  name: string;
  chunk: chunk;
  num_args: int;
  num_locals: int;
  upvalues: value ref array;
}
and typ = {
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
and chunk = {
  mutable code: Opcode.t array;
  mutable constants: value array;
  mutable names: string array;
  mutable handlers: (int * int) list;
}