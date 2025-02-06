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
and type' = {traits: trait list; fields: string list; methods: (string, t) Hashtbl.t}
and trait = {traits: trait list; abs_methods: string list; methods: (string, t) Hashtbl.t}

let null_type = {traits = []; fields = []; methods = Hashtbl.create 16}
let bool_type = {traits = []; fields = []; methods = Hashtbl.create 16}
let number_type = {traits = []; fields = []; methods = Hashtbl.create 16}
let string_type = {traits = []; fields = []; methods = Hashtbl.create 16}
let list_type = {traits = []; fields = []; methods = Hashtbl.create 16}
let dict_type = {traits = []; fields = []; methods = Hashtbl.create 16}
let function_type = {traits = []; fields = []; methods = Hashtbl.create 16}
let type_type = {traits = []; fields = []; methods = Hashtbl.create 16}
let trait_type = {traits = []; fields = []; methods = Hashtbl.create 16}

let make_type fields = {traits = []; fields = fields; methods = Hashtbl.create 16}
let make_trait abs_methods methods = {traits = []; abs_methods = abs_methods; methods = methods |> List.to_seq |> Hashtbl.of_seq}

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
    | None -> get_method_from_traits ts name
    | m -> m
let rec get_method_from_type type' name =
  match Hashtbl.find_opt type'.methods name with
  | Some m -> m
  | None ->
    match get_method_from_traits type'.traits name with
    | Some m -> m
    | None -> failwith ("Method " ^ name ^ " not defined")
let get_method obj name =
  get_method_from_type (type_of obj) name |> bind_self obj
let dot obj name =
  match obj with
  | VStruct s ->
    (match Hashtbl.find_opt s.fields name with
    | Some f -> f
    | None -> get_method obj name)
  | _ -> get_method obj name

let add_methods type' methods = methods |> List.map (fun (n, v) -> (n, VPrimitive v)) |> List.to_seq |> Hashtbl.replace_seq type'.methods;;
add_methods null_type [
  ("to_string", fun _ -> VString "null");
  ("to_bool", fun _ -> VBool false);
];
add_methods bool_type [
  ("to_string", fun [VBool self ] -> VString (string_of_bool self));
  ("to_bool", fun [self] -> self);
  ("and", fun [VBool self; VBool other] -> VBool (self && other));
  ("or", fun [VBool self; VBool other] -> VBool (self || other));
  ("not", fun [VBool self; VBool other] -> VBool (not self));
];
add_methods number_type [
  ("to_string", fun [VNumber self ] -> VString (string_of_float self));
  ("to_bool", fun [VNumber self] -> VBool (self <> 0.0));
  ("+", fun [VNumber self; VNumber other] -> VNumber (self +. other));
  ("-", fun [VNumber self; VNumber other] -> VNumber (self -. other));
  ("*", fun [VNumber self; VNumber other] -> VNumber (self *. other));
  ("/", fun [VNumber self; VNumber other] -> VNumber (self /. other));
  ("%", fun [VNumber self; VNumber other] -> VNumber (mod_float self other));
  ("<", fun [VNumber self; VNumber other] -> VBool (self < other));
  ("<=", fun [VNumber self; VNumber other] -> VBool (self <= other));
  ("==", fun [VNumber self; VNumber other] -> VBool (self = other));
  ("!=", fun [VNumber self; VNumber other] -> VBool (self <> other));
  (">", fun [VNumber self; VNumber other] -> VBool (self > other));
  (">=", fun [VNumber self; VNumber other] -> VBool (self >= other));
];
add_methods string_type [
  ("to_string", fun [self] -> self);
  ("to_bool", fun [VString self] -> VBool (self <> ""));
  ("+", fun [VString self; VString other] -> VString (String.cat self other));
];
add_methods list_type [
  ("to_bool", fun [VList self] -> VBool (self <> []));
  ("head", fun [VList self] ->  List.hd self);
  ("tail", fun [VList self] ->  VList (List.tl self));
  ("at", fun [VList self; VNumber index] -> List.nth self (int_of_float index));
  ("+", fun [VList self; VList other] -> VList (self @ other));
];
add_methods dict_type [
  ("to_bool", fun [VDict self] -> VBool (Hashtbl.length self > 0));
  ("at", fun [VDict self; index] -> Hashtbl.find self index);
];
add_methods function_type [
  ("to_string", fun _ -> VString "<function>");
];
add_methods type_type [
  ("to_string", fun _ -> VString "<type>");
];
add_methods trait_type [
  ("to_string", fun _ -> VString "<trait>");
];