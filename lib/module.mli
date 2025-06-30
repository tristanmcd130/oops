type t = Types.module'

val globals: t
val make: string -> t option -> t
val export: t -> string list -> unit
val find: t -> string -> Value.t option
val add: t -> string -> Value.t -> unit