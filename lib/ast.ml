type t =
| Block of t list
| Null
| Bool of bool
| Number of float
| String of string
| List of t list
| Map of (t * t) list
| Var of string
| Assign of string * t
| Unary of unary_op * t
| Binary of t * binary_op * t
| Fun of string list * t
| Call of t * t list
| If of (t * t) list
and unary_op =
| Negate
| Not
and binary_op =
| Add
| Subtract
| Multiply
| Divide
| Modulo
| LT
| LE
| EQ
| NE
| GT
| GE
| And
| Or
| Cons

let rec to_string = function
| Block b -> "Block([" ^ (b |> List.map to_string |> String.concat ", ") ^ "])"
| Bool b -> "Bool(" ^ string_of_bool b ^ ")"
| Null -> "Null"
| Number n -> "Number(" ^ string_of_float n ^ ")"
| String s -> "String(" ^ s ^ ")"
| List l -> "List([" ^ (l |> List.map to_string |> String.concat ", ") ^ "])"
| Map m -> "Map([" ^ (m |> List.map (fun (k, v) -> "(" ^ to_string k ^ ", " ^ to_string v ^ ")") |> String.concat ", ") ^ "])"
| Var n -> "Var(" ^ n ^ ")"
| Assign (n, v) -> "Assign(" ^ n ^ ", " ^ to_string v ^ ")"
| Unary (o, e) -> "Unary(" ^ (match o with
  | Negate -> "-"
  | Not -> "not") ^ ", " ^ to_string e ^ ")"
| Binary (e1, o, e2) -> "Binary(" ^ to_string e1 ^ ", " ^ (match o with
  | Add -> "+"
  | Subtract -> "-"
  | Multiply -> "*"
  | Divide -> "/"
  | Modulo -> "%"
  | LT -> "<"
  | LE -> "<="
  | EQ -> "=="
  | NE -> "!="
  | GT -> ">"
  | GE -> ">="
  | And -> "and"
  | Or -> "or"
  | Cons -> "::") ^ ", " ^ to_string e2 ^ ")"
| Fun (ps, b) -> "Fun([" ^ (ps |> String.concat ", ") ^ "], " ^ to_string b ^ ")"
| Call (f, a) -> "Call(" ^ to_string f ^ ", [" ^ (a |> List.map to_string |> String.concat ", ") ^ "])"
| If bs -> "If([" ^ (List.map (fun (c, t) -> "(" ^ to_string c ^ ", " ^ to_string t ^ ")") bs |> String.concat ", ") ^ "])"