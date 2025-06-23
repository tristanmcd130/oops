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

val to_string: t -> string