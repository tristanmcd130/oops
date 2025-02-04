type t =
| VNull
| VBool of bool
| VNumber of float
| VString of string
| VList of t list
| VDict of (t, t) Hashtbl.t
| VFunction of string list * Exp.t * t Env.t
| VPrimitive of (t list -> t)
| VObject of {class': class'; fields: (string, t) Hashtbl.t}
| VClass of class'
| VTrait of trait
and class' = {super: class' option; trait: trait; methods: (string, t) Hashtbl.t}
and trait = {abs_methods: string list; methods: (string, t) Hashtbl.t}

let empty_trait = {abs_methods = []; methods = Hashtbl.create 0}
let object_class = {super = None; trait = empty_trait; methods = Hashtbl.create 16}
let null_class = {super = Some object_class; trait = empty_trait; methods = Hashtbl.create 16}
let bool_class = {super = Some object_class; trait = empty_trait; methods = Hashtbl.create 16}
let number_class = {super = Some object_class; trait = empty_trait; methods = Hashtbl.create 16}
let string_class = {super = Some object_class; trait = empty_trait; methods = Hashtbl.create 16}
let list_class = {super = Some object_class; trait = empty_trait; methods = Hashtbl.create 16}
let dict_class = {super = Some object_class; trait = empty_trait; methods = Hashtbl.create 16}
let function_class = {super = Some object_class; trait = empty_trait; methods = Hashtbl.create 16}
let class_class = {super = Some object_class; trait = empty_trait; methods = Hashtbl.create 16}
let trait_class = {super = Some object_class; trait = empty_trait; methods = Hashtbl.create 16}

let rec get_methods class' =
  (class'.methods |> Hashtbl.to_seq_keys |> List.of_seq) @ (class'.trait.methods |> Hashtbl.to_seq_keys |> List.of_seq) @ (match class'.super with Some c -> get_methods c | None -> [])
let uses_trait class' trait =
  let rec subset a b = List.for_all (fun x -> List.mem x b) a in
  subset trait.abs_methods (get_methods class')
let make_class super trait methods =
  let c = {
    super = Some (match super with Some (VClass c) -> c | None -> object_class);
    trait = (match trait with Some (VTrait t) -> t | None -> empty_trait);
    methods = methods |> List.to_seq |> Hashtbl.of_seq
  } in match trait with
  | Some (VTrait t) -> if uses_trait c t then c else failwith ("Class doesn't fully implement trait: " ^ String.concat ", " (List.filter (fun x -> not (List.mem x (get_methods c))) t.abs_methods))
  | None -> c
let make_trait abs_methods methods = {abs_methods = abs_methods; methods = methods |> List.to_seq |> Hashtbl.of_seq}

let class_of = function
| VObject {class' = c} -> c
| VNull -> null_class
| VBool _ -> bool_class
| VNumber _ -> number_class
| VString _ -> string_class
| VList _ -> list_class
| VDict _ -> dict_class
| VFunction _ | VPrimitive _ -> function_class
| VClass _ -> class_class
| VTrait _ -> trait_class
let bind_self obj = function
| VFunction (ps, b, e) -> VFunction (ps, b, Env.create [("self", obj)] (Some e))
| VPrimitive p -> VPrimitive (fun args -> p (obj :: args))
| _ -> failwith "Not a function"
let bind_super class' = function
| VFunction (ps, b, e) ->
  VFunction (ps, b, match class' with
  | Some c -> Env.create [("self", VClass c)] (Some e)
  | None -> e)
| VPrimitive p -> VPrimitive p
| _ -> failwith "Not a function"
let rec get_method_from_class class' name = (* this function is pretty ridiculous *)
  match Hashtbl.find_opt class'.methods name with
  | Some m -> m |> bind_super class'.super
  | None ->
    match Hashtbl.find_opt class'.trait.methods name with
    | Some m -> m |> bind_super class'.super
    | None ->
      match class'.super with
      | Some c -> get_method_from_class c name
      | None -> failwith ("Method " ^ name ^ " not defined")
let get_method obj name =
  get_method_from_class (class_of obj) name |> bind_self obj
let dot obj name =
  match obj with
  | VObject o ->
    (match Hashtbl.find_opt o.fields name with
    | Some f -> f
    | None -> get_method obj name)
  | _ -> get_method obj name

let add_methods class' methods = methods |> List.map (fun (n, v) -> (n, VPrimitive v)) |> List.to_seq |> Hashtbl.replace_seq class'.methods;;
add_methods object_class [("to_string", fun _ -> VString "<object>"); ("init", fun _ -> VNull)];
add_methods null_class [("to_string", fun _ -> VString "null")];
add_methods bool_class [("to_string", fun (VBool self :: _) -> VString (string_of_bool self))];
add_methods number_class [("to_string", fun (VNumber self :: _) -> VString (string_of_float self))];
add_methods string_class [("to_string", fun (self :: _) -> self)];
add_methods function_class [("to_string", fun _ -> VString "<function>")];
add_methods class_class [("to_string", fun _ -> VString "<class>")];
add_methods trait_class [("to_string", fun _ -> VString "<trait>")];