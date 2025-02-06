type 'a type' = {traits: 'a trait list; fields: string list; methods: (string, 'a) Hashtbl.t}
and 'a trait = {traits: 'a trait list; abs_methods: string list; methods: (string, 'a) Hashtbl.t}

module type T = sig
	type 'a t
	val method_names: 'a t -> string list
	val methods: 'a t -> (string, 'a) Hashtbl.t
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
	let impl trait type' methods =
		Hashtbl.replace_seq (T.methods type') (methods |> List.to_seq);
		match trait with
		| Some t -> if not (subset (T.method_names type') t.abs_methods) then failwith "Trait not implemented fully"
		| None -> ()
end