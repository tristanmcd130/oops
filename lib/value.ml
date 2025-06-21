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
and 'a func = {
  chunk: 'a;
  num_args: int;
  num_locals: int;
}
and 'a closure = {
  func: 'a func;
  upvalues: 'a t ref array;
}

let make_function chunk num_args num_locals = {chunk; num_args; num_locals}
let make_closure func upvalues = {func; upvalues}
let chunk closure = closure.func.chunk
let num_args closure = closure.func.num_args
let num_locals closure = closure.func.num_locals
let get_upvalue closure i = closure.upvalues.(i)
let rec to_string = function
| Null -> "null"
| Bool b -> string_of_bool b
| Number n -> Printf.sprintf "%g" n
| String s -> s
| List l -> "[" ^ (List.map to_string l |> String.concat ", ") ^ "]"
| Map m -> "{" ^ (m |> Hashtbl.to_seq |> List.of_seq |> List.map (fun (k, v) -> to_string k ^ ": " ^ to_string v) |> String.concat ", ") ^ "}"
| Function f -> "<function with " ^ string_of_int f.num_args ^ " args and " ^ string_of_int f.num_locals ^ " locals>"
| Closure c -> "<closure>"
| Cell c -> "<cell containing " ^ to_string !c ^ ">"