type t

val make: unit -> t
val call: t -> Value.closure -> Value.t list -> Value.t