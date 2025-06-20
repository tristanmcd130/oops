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

let make_func chunk num_args num_locals = {chunk; num_args; num_locals}
let make_closure func upvalues = {func; upvalues}
let rec to_string = function
| Null -> "null"
| Bool b -> string_of_bool b
| Number n -> Printf.sprintf "%g" n
| Function f -> "<function>"
| Closure c -> "<closure>"
| String s -> s
| List l -> "[" ^ (List.map to_string l |> String.concat ", ") ^ "]"
| Map m -> "{" ^ (m |> Hashtbl.to_seq |> List.of_seq |> List.map (fun (k, v) -> to_string k ^ ": " ^ to_string v) |> String.concat ", ") ^ "}"