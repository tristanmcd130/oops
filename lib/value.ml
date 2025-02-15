type t =
| VNull
| VBool of bool
| VNumber of float
| VString of string
| VList of t list
| VDict of (t, t) Hashtbl.t
| VFunction of string list * Exp.t * t Env.t
| VPrimitive of (t list -> t)
| VStruct of {type': type'; fields: (string, t) Hashtbl.t}
| VType of type'
| VTrait of trait
and type' = {mutable traits: trait list; fields: string list; methods: (string, t) Hashtbl.t}
and trait = {abs_methods: string list; methods: (string, t) Hashtbl.t}

let null_type = {traits = []; fields = []; methods = Hashtbl.create 16}
let bool_type = {traits = []; fields = []; methods = Hashtbl.create 16}
let number_type = {traits = []; fields = []; methods = Hashtbl.create 16}
let string_type = {traits = []; fields = []; methods = Hashtbl.create 16}
let list_type = {traits = []; fields = []; methods = Hashtbl.create 16}
let dict_type = {traits = []; fields = []; methods = Hashtbl.create 16}
let function_type = {traits = []; fields = []; methods = Hashtbl.create 16}
let type_type = {traits = []; fields = []; methods = Hashtbl.create 16}
let trait_type = {traits = []; fields = []; methods = Hashtbl.create 16}

let make_type fields = VType {traits = []; fields = fields; methods = Hashtbl.create 16}
let make_trait abs_methods methods = VTrait {abs_methods = abs_methods; methods = methods |> List.to_seq |> Hashtbl.of_seq}
let make_struct type' args = VStruct {type' = type'; fields = List.combine type'.fields args |> List.to_seq |> Hashtbl.of_seq}

let type_of = function
| VStruct {type' = t} -> t
| VNull -> null_type
| VBool _ -> bool_type
| VNumber _ -> number_type
| VString _ -> string_type
| VList _ -> list_type
| VDict _ -> dict_type
| VFunction _ | VPrimitive _ -> function_type
| VType _ -> type_type
| VTrait _ -> trait_type
let bind_self obj = function
| VFunction (ps, b, e) -> VFunction (ps, b, Env.create [("self", obj)] (Some e))
| VPrimitive p -> VPrimitive (fun args -> p (obj :: args))
| _ -> failwith "Not a function"
let rec get_method_from_traits (traits: trait list) name =
  match traits with
  | [] -> None
  | t :: ts ->
    match Hashtbl.find_opt t.methods name with
    | Some m -> Some m
    | None -> get_method_from_traits ts name
let get_method_from_type type' name =
  match Hashtbl.find_opt type'.methods name with
  | Some m -> m
  | None ->
    match get_method_from_traits type'.traits name with
    | Some m -> m
    | None -> failwith ("Method " ^ name ^ " not defined")
let get_method value name = get_method_from_type (type_of value) name |> bind_self value
let dot value name =
  match value with
  | VStruct s ->
    (match Hashtbl.find_opt s.fields name with
    | Some f -> f
    | None -> get_method value name)
  | v -> get_method v name

let subset a b = List.for_all (fun x -> List.mem x b) a
let trait_method_names (trait: trait) = trait.methods |> Hashtbl.to_seq_keys |> List.of_seq
let method_names type' =
  (type'.methods |> Hashtbl.to_seq_keys |> List.of_seq) @ List.concat_map trait_method_names type'.traits
let impl trait (VType type') methods =
  match trait with
  | Some (VTrait t) ->
    if subset t.abs_methods (method_names type' @ List.map fst methods) then
      (type'.traits <- t :: type'.traits;
      Hashtbl.replace_seq type'.methods (methods |> List.to_seq))
    else
      failwith ("Trait not implemented fully: " ^ String.concat ", " (List.filter (fun x -> not (List.mem x (method_names type' @ List.map fst methods))) t.abs_methods))
  | None ->
    Hashtbl.replace_seq type'.methods (methods |> List.to_seq);;

impl None (VType null_type) [
  ("to_string", VPrimitive (fun _ -> VString "null"));
  ("to_bool", VPrimitive (fun _ -> VBool false));
];
impl None (VType bool_type) [
  ("to_string", VPrimitive (fun [VBool self ] -> VString (string_of_bool self)));
  ("to_bool", VPrimitive (fun [self] -> self));
  ("and", VPrimitive (fun [VBool self; VBool other] -> VBool (self && other)));
  ("or", VPrimitive (fun [VBool self; VBool other] -> VBool (self || other)));
  ("not", VPrimitive (fun [VBool self; VBool other] -> VBool (not self)));
];
impl None (VType number_type) [
  ("to_string", VPrimitive (fun [VNumber self ] -> VString (string_of_float self)));
  ("to_bool", VPrimitive (fun [VNumber self] -> VBool (self <> 0.0)));
  ("+", VPrimitive (fun [VNumber self; VNumber other] -> VNumber (self +. other)));
  ("-", VPrimitive (fun [VNumber self; VNumber other] -> VNumber (self -. other)));
  ("*", VPrimitive (fun [VNumber self; VNumber other] -> VNumber (self *. other)));
  ("/", VPrimitive (fun [VNumber self; VNumber other] -> VNumber (self /. other)));
  ("%", VPrimitive (fun [VNumber self; VNumber other] -> VNumber (mod_float self other)));
  ("<", VPrimitive (fun [VNumber self; VNumber other] -> VBool (self < other)));
  ("<=", VPrimitive (fun [VNumber self; VNumber other] -> VBool (self <= other)));
  ("==", VPrimitive (fun [VNumber self; VNumber other] -> VBool (self = other)));
  ("!=", VPrimitive (fun [VNumber self; VNumber other] -> VBool (self <> other)));
  (">", VPrimitive (fun [VNumber self; VNumber other] -> VBool (self > other)));
  (">=", VPrimitive (fun [VNumber self; VNumber other] -> VBool (self >= other)));
];
impl None (VType string_type) [
  ("to_string", VPrimitive (fun [self] -> self));
  ("to_bool", VPrimitive (fun [VString self] -> VBool (self <> "")));
  ("+", VPrimitive (fun [VString self; VString other] -> VString (String.cat self other)));
];
impl None (VType list_type) [
  ("to_bool", VPrimitive (fun [VList self] -> VBool (self <> [])));
  ("head", VPrimitive (fun [VList self] ->  List.hd self));
  ("tail", VPrimitive (fun [VList self] ->  VList (List.tl self)));
  ("length", VPrimitive (fun [VList self] ->  VNumber (List.length self |> float_of_int)));
  ("at", VPrimitive (fun [VList self; VNumber index] -> List.nth self (int_of_float index)));
  ("+", VPrimitive (fun [VList self; VList other] -> VList (self @ other)));
  ("::", VPrimitive (fun [VList self; other] -> VList (other :: self)));
];
impl None (VType dict_type) [
  ("to_bool", VPrimitive (fun [VDict self] -> VBool (Hashtbl.length self > 0)));
  ("at", VPrimitive (fun [VDict self; index] -> Hashtbl.find self index));
];
impl None (VType function_type) [
  ("to_string", VPrimitive (fun _ -> VString "<function>"));
];
impl None (VType type_type) [
  ("to_string", VPrimitive (fun _ -> VString "<type>"));
];
impl None (VType trait_type) [
  ("to_string", VPrimitive (fun _ -> VString "<trait>"));
];