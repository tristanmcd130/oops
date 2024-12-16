open Exp

val eval: Ast.t -> Env.t -> Exp.t
val string_of_exp: Exp.t -> string
val run_file: string -> Env.t -> Exp.t