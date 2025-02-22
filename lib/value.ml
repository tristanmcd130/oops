type t =
| VNull
| VBool of bool
| VNumber of float
| VString of string
| VList of t list
| VDict of (t, t) Hashtbl.t
| VFunction of string * string list * Exp.t * t Env.t
| VPrimitive of (t list -> t)
| VStruct of {type': type'; fields: (string, t) Hashtbl.t}
| VType of type'
| VTrait of trait
and type' = {name: string; mutable traits: trait list; fields: string list; methods: (string, t) Hashtbl.t}
and trait = {name: string; mutable traits: trait list; abs_methods: string list; methods: (string, t) Hashtbl.t}

let null_type = {name = "Null"; traits = []; fields = []; methods = Hashtbl.create 16}
let bool_type = {name = "Bool"; traits = []; fields = []; methods = Hashtbl.create 16}
let number_type = {name = "Number"; traits = []; fields = []; methods = Hashtbl.create 16}
let string_type = {name = "String"; traits = []; fields = []; methods = Hashtbl.create 16}
let list_type = {name = "List"; traits = []; fields = []; methods = Hashtbl.create 16}
let dict_type = {name = "Dict"; traits = []; fields = []; methods = Hashtbl.create 16}
let function_type = {name = "Function"; traits = []; fields = []; methods = Hashtbl.create 16}
let type_type = {name = "Type"; traits = []; fields = []; methods = Hashtbl.create 16}
let trait_type = {name = "Trait"; traits = []; fields = []; methods = Hashtbl.create 16}
let module_type = {name = "Module"; traits = []; fields = []; methods = Hashtbl.create 16}
let printable_trait = {name = "Printable"; traits = []; abs_methods = ["to_string"]; methods = Hashtbl.create 16}

let type_name = function
| VType t -> t.name
| VTrait t-> t.name
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
| VFunction (n, ps, b, e) -> VFunction (n, ps, b, Env.create [("self", obj)] (Some e))
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
    | None -> failwith (type'.name ^ " does not define method " ^ name)
let get_method value name = get_method_from_type (type_of value) name |> bind_self value
let dot value name =
  match value with
  | VStruct s ->
    (match Hashtbl.find_opt s.fields name with
    | Some f -> f
    | None -> get_method value name)
  | v -> get_method v name

let subset a b = List.for_all (fun x -> List.mem x b) a
let rec method_names = function
| VType t -> (t.methods |> Hashtbl.to_seq_keys |> List.of_seq) @ (t.traits |> List.map (fun x -> VTrait x) |> List.concat_map method_names)
| VTrait t -> (t.methods |> Hashtbl.to_seq_keys |> List.of_seq) @ (t.traits |> List.map (fun x -> VTrait x) |> List.concat_map method_names)
let add_trait type' trait =
  match type' with
  | VType t -> t.traits <- trait :: t.traits
  | VTrait t -> t.traits <- trait :: t.traits
let add_methods type' methods =
  match type' with
  | VType t -> Hashtbl.replace_seq t.methods (methods |> List.to_seq)
  | VTrait t -> Hashtbl.replace_seq t.methods (methods |> List.to_seq)
let impl trait type' methods =
  match trait with
  | Some t ->
    if subset t.abs_methods (method_names type' @ List.map fst methods) then
      (add_trait type' t;
      add_methods type' methods)
    else
      failwith (type_name type' ^ " does not fully implement " ^ t.name ^ ": " ^ String.concat ", " (List.filter (fun x -> not (List.mem x (method_names type' @ List.map fst methods))) t.abs_methods))
  | None -> add_methods type' methods;;

impl (Some printable_trait) (VType null_type) [
  ("to_string", VPrimitive (fun _ -> VString "null"));
];
impl None (VType bool_type) [
  ("and", VPrimitive (fun [VBool self; VBool other] -> VBool (self && other)));
  ("or", VPrimitive (fun [VBool self; VBool other] -> VBool (self || other)));
  ("not", VPrimitive (fun [VBool self; VBool other] -> VBool (not self)));
];
impl (Some printable_trait) (VType bool_type) [
  ("to_string", VPrimitive (fun [VBool self ] -> VString (string_of_bool self)));
];
impl None (VType number_type) [
  ("+", VPrimitive (fun [VNumber self; VNumber other] -> VNumber (self +. other)));
  ("-", VPrimitive (fun [VNumber self; VNumber other] -> VNumber (self -. other)));
  ("u-", VPrimitive (fun [VNumber self] -> VNumber ~-.self));
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
impl (Some printable_trait) (VType number_type) [
  ("to_string", VPrimitive (fun [VNumber self ] -> VString (Printf.sprintf "%g" self)));
];
impl None (VType string_type) [
  ("to_string", VPrimitive (fun [self] -> self));
  ("+", VPrimitive (fun [VString self; VString other] -> VString (String.cat self other)));
  ("head", VPrimitive (fun [VString self] ->  VString (self.[0] |> String.make 1)));
  ("tail", VPrimitive (fun [VString self] ->  VString (String.sub self 1 (String.length self - 1))));
  ("length", VPrimitive (fun [VString self] ->  VNumber (String.length self |> float_of_int)));
];
impl (Some printable_trait) (VType string_type) [
  ("to_string", VPrimitive (fun [self] -> self));
];
impl None (VType list_type) [
  ("head", VPrimitive (fun [VList self] ->  List.hd self));
  ("tail", VPrimitive (fun [VList self] ->  VList (List.tl self)));
  ("length", VPrimitive (fun [VList self] ->  VNumber (List.length self |> float_of_int)));
  ("at", VPrimitive (fun [VList self; VNumber index] -> List.nth self (int_of_float index)));
  ("+", VPrimitive (fun [VList self; VList other] -> VList (self @ other)));
  ("::", VPrimitive (fun [VList self; other] -> VList (other :: self)));
];
impl None (VType dict_type) [
  ("at", VPrimitive (fun [VDict self; index] -> Hashtbl.find self index));
  ("pairs", VPrimitive (fun [VDict self] -> VList (Hashtbl.to_seq self |> List.of_seq |> List.map (fun (k, v) -> VList [k; v])));)
];
impl (Some printable_trait) (VType function_type) [
  ("to_string", VPrimitive (function [VFunction (n, _, _, _)] -> VString ("<function" ^ (if n = "" then "" else " " ^ n) ^ ">") | [VPrimitive _] -> VString "<primitive>"));
];
impl (Some printable_trait) (VType type_type) [
  ("to_string", VPrimitive (fun [VType t] -> VString ("<type " ^ t.name ^ ">")));
];
impl (Some printable_trait) (VType trait_type) [
  ("to_string", VPrimitive (fun [VTrait t] -> VString ("<trait " ^ t.name ^ ">")));
];