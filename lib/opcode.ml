type t =
| GetConstant of int
| MakeList of int
| MakeMap of int

let to_string = function
| GetConstant i -> "get_constant " ^ string_of_int i
| MakeList i -> "make_list " ^ string_of_int i
| MakeMap i -> "make_map " ^ string_of_int i