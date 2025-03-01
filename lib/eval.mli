exception Runtime_error of Value.t

val eval: Exp.t -> Value.t Env.t -> Value.t
val to_string: Value.t -> string
val run_file: string -> Value.t Env.t -> unit