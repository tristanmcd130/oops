type 'a t

val create: (string * 'a) list -> 'a t option -> 'a t
val lookup: 'a t -> string -> 'a
val bind: 'a t -> string -> 'a -> unit