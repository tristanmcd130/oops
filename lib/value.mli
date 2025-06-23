type 'a t =
| Null
| Bool of bool
| Number of float
| String of string
| List of 'a t list
| Map of ('a t, 'a t) Hashtbl.t
| Closure of 'a closure
| Cell of 'a t ref
| Primitive of string * ('a t list -> 'a t)
| Struct of 'a typ * (string, 'a t) Hashtbl.t
| Type of 'a typ
| Trait of 'a trait
| Method of 'a meth
and 'a closure = {
  name: string;
  chunk: 'a;
  num_args: int;
  num_locals: int;
  upvalues: 'a t ref array;
}
and 'a typ = {
  name: string;
  fields: string list;
  methods: (string, 'a t) Hashtbl.t;
  mutable traits: 'a trait list;
}
and 'a trait = {
  name: string;
  requires: string list;
  provides: (string, 'a t) Hashtbl.t;
}
and 'a meth = {
  closure: 'a closure;
  self: 'a t;
}

val to_string: 'a t -> string