type t =
| Block of t list
| Null
| Bool of bool
| Number of float
| String of string
| List of t list
| Map of (t * t) list

let rec to_string = function
| Block b -> "Block([" ^ (b |> List.map to_string |> String.concat ", ") ^ "])"
| Bool b -> "Bool(" ^ string_of_bool b ^ ")"
| Null -> "Null"
| Number n -> "Number(" ^ string_of_float n ^ ")"
| String s -> "String(" ^ s ^ ")"
| List l -> "List([" ^ (l |> List.map to_string |> String.concat ", ") ^ "])"
| Map m -> "Map([" ^ (m |> List.map (fun (k, v) -> "(" ^ to_string k ^ ", " ^ to_string v ^ ")") |> String.concat ", ") ^ "])"