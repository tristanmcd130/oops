type 'a t =
| Null
| Bool of bool
| Number of float
| String of string
| List of 'a t list
| Map of ('a t, 'a t) Hashtbl.t
| Closure of 'a closure
| Cell of 'a t ref
| Primitive of ('a t list -> 'a t)
| Struct of 'a typ * 'a t array
| Type of 'a typ
| Trait of 'a trait
| Method of 'a t * 'a closure
and 'a closure = {
  name: string;
  chunk: 'a;
  num_args: int;
  num_locals: int;
  upvalues: 'a t ref array;
}
and 'a typ = {
  name: string;
  fields: (string, int) Hashtbl.t;
  methods: (string, 'a t) Hashtbl.t;
  mutable traits: 'a trait list;
}
and 'a trait = {
  name: string;
  requires: string list;
  provides: (string, 'a t) Hashtbl.t;
}

val to_string: 'a t -> string