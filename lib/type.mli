module type T = sig
	type t
	type value
	type trait
	val method_names: t -> string list (* just that specific type, not any of its traits *)
	val methods: t -> (string, value) Hashtbl.t (* same here *)
	val abs_methods: trait -> string list
	val add_trait: t -> trait -> unit
end

module Make: functor(T: T) -> sig
	val get_method: T.t list -> string -> T.value option
	val impl: T.trait option -> T.t -> (string * T.value) list -> unit
end