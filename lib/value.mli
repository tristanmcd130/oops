type 'a t =
| Null
| Bool of bool
| Number of float
| String of string
| List of 'a t list
| Map of ('a t, 'a t) Hashtbl.t
| Function of 'a func
| Closure of 'a closure
| Cell of 'a t ref
| Primitive of ('a t list -> 'a t)
and 'a func = {
  chunk: 'a;
  num_args: int;
  num_locals: int;
}
and 'a closure = {
  func: 'a func;
  upvalues: 'a t ref array;
}

val to_string: 'a t -> string