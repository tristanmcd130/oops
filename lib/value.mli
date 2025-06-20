type 'a t =
| Null
| Bool of bool
| Number of float
| String of string
| List of 'a t list
| Map of ('a t, 'a t) Hashtbl.t
| Function of 'a func
| Closure of 'a closure
and 'a func = {
  chunk: 'a;
  num_args: int;
  num_locals: int;
}
and 'a closure = {
  func: 'a func;
  upvalues: 'a t ref array;
}

val make_func: 'a -> int -> int -> 'a func
val make_closure: 'a func -> 'a t ref array -> 'a closure
val to_string: 'a t -> string