module type T = sig
	type t
	type value
	type trait
	val method_names: t -> string list (* just that specific type, not any of its traits *)
	val methods: t -> (string, value) Hashtbl.t (* same here *)
	val abs_methods: trait -> string list
	val add_trait: t -> trait -> unit
end

let subset a b = List.for_all (fun x -> List.mem x b) a

module Make(T: T) = struct
	let rec get_method types name =
		match types with
		| [] -> None
		| t :: ts ->
			match Hashtbl.find_opt (T.methods t) name with
			| None -> get_method ts name
			| m -> m
	let impl (trait: T.trait option) type' methods =
		match trait with
		| Some t ->
			if subset (T.method_names type' @ List.map fst methods) (T.abs_methods t) then
				T.add_trait type' t
			else
				failwith "Trait not implemented fully"
		| None -> ();
		Hashtbl.replace_seq (T.methods type') (methods |> List.to_seq)
end