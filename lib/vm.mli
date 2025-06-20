type t

val make: (string, Chunk.t Value.t) Hashtbl.t -> t
val add_global: t -> string -> Chunk.t Value.t -> unit
val call: t -> Chunk.t Value.closure -> Chunk.t Value.t list -> Chunk.t Value.t