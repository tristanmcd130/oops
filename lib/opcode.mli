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

val to_string: t -> string