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
| LT
| LE
| EQ
| NE
| GT
| GE
| And
| Or
| Not
| Cons
| GetLocal of int
| SetLocal of int
| Call of int
| MakeCell of int
| MakeClosure of int
| GetUpvalue of int
| DerefUpvalue of int
| Jump of int
| JumpIfFalse of int

val to_string: t -> string