type t

val make: (string, Value.t) Hashtbl.t -> t
val add_global: t -> string -> Value.t -> unit
val call: t -> Value.closure -> Value.t list -> Value.t