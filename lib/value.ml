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

let rec to_string = function
| Null -> "null"
| Bool b -> string_of_bool b
| Number n -> Printf.sprintf "%g" n
| String s -> s
| List l -> "[" ^ (List.map to_string l |> String.concat ", ") ^ "]"
| Map m -> "{" ^ (m |> Hashtbl.to_seq |> List.of_seq |> List.map (fun (k, v) -> to_string k ^ ": " ^ to_string v) |> String.concat ", ") ^ "}"
| Closure {name = ""} -> "<anonymous function>"
| Closure {name} -> "<function " ^ name ^ ">"
| Cell c -> "<cell containing " ^ to_string !c ^ ">"
| Primitive (n, _) -> "<primitive " ^ n ^ ">"
| Struct (t, fs) -> t.name ^ "(" ^ (List.map (fun n -> n |> Hashtbl.find fs |> to_string) t.fields |> String.concat ", ") ^ ")"
| Type t -> "<type " ^ t.name ^ ">"
| Trait t -> "<trait " ^ t.name ^ ">"
| Method m -> to_string (Closure m.closure)