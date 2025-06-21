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
and 'a func
and 'a closure

val make_function: 'a -> int -> int -> 'a func
val make_closure: 'a func -> 'a t ref array -> 'a closure
val chunk: 'a closure -> 'a
val num_args: 'a closure -> int
val num_locals: 'a closure -> int
val get_upvalue: 'a closure -> int -> 'a t ref
val to_string: 'a t -> string