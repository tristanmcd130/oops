type t =
| GetConstant of int
| MakeList of int
| MakeMap of int
| GetGlobal of int
| SetGlobal of int
| GetLocal of int
| SetLocal of int
| Call of int
| MakeCell of int
| Enclose of int
| GetUpvalue of int
| DerefUpvalue of int
| Jump of int
| JumpIfFalse of int
| Dot of int
| AddMethod of int
| Impl
| BaseTrait
| Import of int
| DupDot of int
| TailCall of int

let to_string = function
| GetConstant i -> "get_constant " ^ string_of_int i
| MakeList i -> "make_list " ^ string_of_int i
| MakeMap i -> "make_map " ^ string_of_int i
| GetGlobal i -> "get_global " ^ string_of_int i
| SetGlobal i -> "set_global " ^ string_of_int i
| GetLocal i -> "get_local " ^ string_of_int i
| SetLocal i -> "set_local " ^ string_of_int i
| Call i -> "call " ^ string_of_int i
| MakeCell i -> "make_cell " ^ string_of_int i
| Enclose i -> "enclose " ^ string_of_int i
| GetUpvalue i -> "get_upvalue " ^ string_of_int i
| DerefUpvalue i -> "deref_upvalue " ^ string_of_int i
| Jump i -> "jump " ^ string_of_int i
| JumpIfFalse i -> "jump_if_false " ^ string_of_int i
| Dot i -> "dot " ^ string_of_int i
| AddMethod i -> "add_method " ^ string_of_int i
| Impl -> "impl"
| BaseTrait -> "base_trait"
| Import i -> "import " ^ string_of_int i
| DupDot i -> "dup_dot " ^ string_of_int i
| TailCall i -> "tall_call " ^ string_of_int i