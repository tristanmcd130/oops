type t =
| GetConstant of int
| MakeList of int
| MakeMap of int
| GetGlobal of int
| SetGlobal of int
| Negate
| Add
| Subtract
| Multiply
| Divide
| Modulo
| GetLocal of int
| SetLocal of int
| Call of int
| MakeCell of int
| MakeClosure of int
| GetUpvalue of int
| DerefUpvalue of int

let to_string = function
| GetConstant i -> "get_constant " ^ string_of_int i
| MakeList i -> "make_list " ^ string_of_int i
| MakeMap i -> "make_map " ^ string_of_int i
| GetGlobal i -> "get_global " ^ string_of_int i
| SetGlobal i -> "set_global " ^ string_of_int i
| Negate -> "negate"
| Add -> "add"
| Subtract -> "subtract"
| Multiply -> "multiply"
| Divide -> "divide"
| Modulo -> "modulo"
| GetLocal i -> "get_local " ^ string_of_int i
| SetLocal i -> "set_local " ^ string_of_int i
| Call i -> "call " ^ string_of_int i
| MakeCell i -> "make_cell " ^ string_of_int i
| MakeClosure i -> "make_closure " ^ string_of_int i
| GetUpvalue i -> "get_upvalue " ^ string_of_int i
| DerefUpvalue i -> "deref_upvalue " ^ string_of_int i