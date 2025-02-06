type 'a type'
and 'a trait

module type T = sig
	type 'a t
	val method_names: 'a t -> string list
	val methods: 'a t -> (string, 'a) Hashtbl.t
end

module Make: functor(T: T) -> sig
	val impl: 'a trait option -> 'a T.t -> (string * 'a) list -> unit
end