type t =
| Null
| Bool of bool
| Number of float
| String of string
| List of t list
| Dict of (t, t) Hashtbl.t
| Function of string * string list * Exp.t * t Env.t
| Primitive of (t list -> t)
| Struct of (type' * (string, t) Hashtbl.t)
| Type of type'
| Trait of trait
and type' = {name: string; mutable traits: trait list; fields: string list; methods: (string, t) Hashtbl.t}
and trait = {name: string; mutable traits: trait list; abs_methods: string list; methods: (string, t) Hashtbl.t}

exception Runtime_error of t

let base_trait = {name = "Base"; traits = []; abs_methods = []; methods = Hashtbl.create 16}
let null_type = {name = "Null"; traits = [base_trait]; fields = []; methods = Hashtbl.create 16}
let bool_type = {name = "Bool"; traits = [base_trait]; fields = []; methods = Hashtbl.create 16}
let number_type = {name = "Number"; traits = [base_trait]; fields = []; methods = Hashtbl.create 16}
let string_type = {name = "String"; traits = [base_trait]; fields = []; methods = Hashtbl.create 16}
let list_type = {name = "List"; traits = [base_trait]; fields = []; methods = Hashtbl.create 16}
let dict_type = {name = "Dict"; traits = [base_trait]; fields = []; methods = Hashtbl.create 16}
let function_type = {name = "Function"; traits = [base_trait]; fields = []; methods = Hashtbl.create 16}
let type_type = {name = "Type"; traits = [base_trait]; fields = []; methods = Hashtbl.create 16}
let trait_type = {name = "Trait"; traits = [base_trait]; fields = []; methods = Hashtbl.create 16}
let module_type = {name = "Module"; traits = [base_trait]; fields = []; methods = Hashtbl.create 16}
let error_trait = {name = "Error"; traits = []; abs_methods = ["message"]; methods = Hashtbl.create 16}
let field_undefined_error_type = {name = "FieldUndefinedError"; traits = [error_trait; base_trait]; fields = ["msg"]; methods = Hashtbl.create 16}
let trait_not_implemented_error_type = {name = "TraitNotImplementedError"; traits = [error_trait; base_trait]; fields = ["msg"]; methods = Hashtbl.create 16}
let variable_undefined_error_type = {name = "VariableUndefinedError"; traits = [error_trait; base_trait]; fields = ["msg"]; methods = Hashtbl.create 16}
let primitive_error_type = {name = "PrimitiveError"; traits = [error_trait; base_trait]; fields = ["msg"]; methods = Hashtbl.create 16}
let printable_trait = {name = "Printable"; traits = []; abs_methods = ["to_string"]; methods = Hashtbl.create 16}

let make_type name fields = {name = name; traits = [base_trait]; fields = fields; methods = Hashtbl.create 16}
let make_trait name abs_methods methods = {name = name; traits = []; abs_methods = abs_methods; methods = methods |> List.to_seq |> Hashtbl.of_seq}
let make_struct type' args = Struct (type', List.combine type'.fields args |> List.to_seq |> Hashtbl.of_seq)
let throw error_type msg = raise (Runtime_error (make_struct error_type [String msg]))

let fields type' = type'.fields
let type_name = function
| Type t -> t.name
| Trait t-> t.name
let type_of = function
| Struct (t, _) -> t
| Null -> null_type
| Bool _ -> bool_type
| Number _ -> number_type
| String _ -> string_type
| List _ -> list_type
| Dict _ -> dict_type
| Function _ | Primitive _ -> function_type
| Type _ -> type_type
| Trait _ -> trait_type
let bind_self obj = function
| Function (n, ps, b, e) -> Function (n, ps, b, Env.create [("self", obj)] (Some e))
| Primitive p -> Primitive (fun args -> p (obj :: args))
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
    | None -> throw field_undefined_error_type (type'.name ^ " does not define method " ^ name)
let get_method value name = get_method_from_type (type_of value) name |> bind_self value
let dot value name =
  match value with
  | Struct (_, fs) ->
    (match Hashtbl.find_opt fs name with
    | Some f -> f
    | None -> get_method value name)
  | v -> get_method v name
let dot_assign obj field value =
  match obj with
  | Struct (t, fs) ->
    (if Hashtbl.mem fs field then
      Hashtbl.replace fs field value
    else
      throw field_undefined_error_type (t.name ^ " does not have field " ^ field))
  | _ -> throw field_undefined_error_type "Primitive values have no fields"

let rec make_literal = function
| Exp.Bool b -> Bool b
| Number n -> Number n
| String s -> String s
| List l -> List (List.map make_literal l)
| Dict d -> Dict (List.map (fun (k, v) -> (make_literal k, make_literal v)) d |> List.to_seq |> Hashtbl.of_seq)
| _ -> failwith "match fails on dicts containing non-literals"
let rec match' pattern value =
  match (pattern, value) with
  | (Exp.Null, Null) -> Some []
  | (Bool b, Bool b') when b = b' -> Some []
  | (Number n, Number n') when n = n' -> Some []
  | (String s, String s') when s = s' -> Some []
  | (List [], List []) -> Some []
  | (List (p :: ps), List (v :: vs)) ->
    (match match' p v with
    | Some bs ->
      (match match' (List ps) (List vs) with
      | Some bs' -> Some (bs' @ bs)
      | None -> None)
    | None -> None)
  | (Dict [], _) -> Some []
  | (Dict ((k, v) :: d), Dict d') ->
    (match Hashtbl.find_opt d' (make_literal k) with
    | Some v' ->
      (match match' v v' with
      | Some bs ->
        (match match' (Dict d) (Dict d') with
        | Some bs' -> Some (bs' @ bs)
        | None -> None)
      | None -> None)
    | None -> None)
  | (Var "_", _) -> Some []
  | (Var n, v) -> Some [(n, v)]
  | (Call (Dot (ps, "::"), [p]), List (v :: vs)) ->
    (match match' p v with
    | Some bs ->
      (match match' ps (List vs) with
      | Some bs' -> Some (bs' @ bs)
      | None -> None)
    | None -> None)
  | (Call (Var s, ps), Struct (t, fs)) when s = t.name -> match' (List ps) (List (List.map (Hashtbl.find fs) t.fields))
  | (Call (Dot (p1, "or"), [p2]), v) ->
    (match match' p1 v with
    | Some bs -> Some bs
    | None -> match' p2 v)
  | (Call (Dot (p1, "and"), [p2]), v) ->
    (match match' p1 v with
    | Some bs ->
      (match match' p2 v with
      | Some bs' -> Some (bs' @ bs)
      | None -> None)
    | None -> None)
  | _ -> None

let subset a b = List.for_all (fun x -> List.mem x b) a
let rec method_names = function
| Type t -> (t.methods |> Hashtbl.to_seq_keys |> List.of_seq) @ (t.traits |> List.map (fun x -> Trait x) |> List.concat_map method_names)
| Trait t -> (t.methods |> Hashtbl.to_seq_keys |> List.of_seq) @ (t.traits |> List.map (fun x -> Trait x) |> List.concat_map method_names)
let add_trait type' trait =
  match type' with
  | Type t -> t.traits <- trait :: t.traits
  | Trait t -> t.traits <- trait :: t.traits
let add_methods type' methods =
  match type' with
  | Type t -> Hashtbl.replace_seq t.methods (methods |> List.to_seq)
  | Trait t -> Hashtbl.replace_seq t.methods (methods |> List.to_seq)
let impl trait type' methods =
  match trait with
  | Some t ->
    if subset t.abs_methods (method_names type' @ List.map fst methods) then
      (add_trait type' t;
      add_methods type' methods)
    else
      throw trait_not_implemented_error_type (type_name type' ^ " does not fully implement " ^ t.name ^ ": " ^ String.concat ", " (List.filter (fun x -> not (List.mem x (method_names type' @ List.map fst methods))) t.abs_methods))
  | None -> add_methods type' methods;;

impl (Some printable_trait) (Type null_type) [
  ("to_string", Primitive (fun [_] -> String "null"));
];
impl None (Type bool_type) [
  ("and", Primitive (fun [Bool self; Bool other] -> Bool (self && other)));
  ("or", Primitive (fun [Bool self; Bool other] -> Bool (self || other)));
  ("not", Primitive (fun [Bool self] -> Bool (not self)));
];
impl (Some printable_trait) (Type bool_type) [
  ("to_string", Primitive (fun [Bool self ] -> String (string_of_bool self)));
];
impl None (Type number_type) [
  ("+", Primitive (fun [Number self; Number other] -> Number (self +. other)));
  ("-", Primitive (fun [Number self; Number other] -> Number (self -. other)));
  ("u-", Primitive (fun [Number self] -> Number ~-.self));
  ("*", Primitive (fun [Number self; Number other] -> Number (self *. other)));
  ("/", Primitive (fun [Number self; Number other] -> Number (self /. other)));
  ("%", Primitive (fun [Number self; Number other] -> Number (mod_float self other)));
  ("<", Primitive (fun [Number self; Number other] -> Bool (self < other)));
  ("<=", Primitive (fun [Number self; Number other] -> Bool (self <= other)));
  ("!=", Primitive (fun [Number self; Number other] -> Bool (self <> other)));
  (">", Primitive (fun [Number self; Number other] -> Bool (self > other)));
  (">=", Primitive (fun [Number self; Number other] -> Bool (self >= other)));
];
impl (Some printable_trait) (Type number_type) [
  ("to_string", Primitive (fun [Number self ] -> String (Printf.sprintf "%g" self)));
];
impl None (Type string_type) [
  ("+", Primitive (fun [String self; String other] -> String (String.cat self other)));
  ("head", Primitive (fun [String self] ->  String (self.[0] |> String.make 1)));
  ("tail", Primitive (fun [String self] ->  String (String.sub self 1 (String.length self - 1))));
  ("length", Primitive (fun [String self] ->  Number (String.length self |> float_of_int)));
];
impl (Some printable_trait) (Type string_type) [
  ("to_string", Primitive (fun [self] -> self));
];
impl None (Type list_type) [
  ("head", Primitive (fun [List self] ->  List.hd self));
  ("tail", Primitive (fun [List self] ->  List (List.tl self)));
  ("length", Primitive (fun [List self] ->  Number (List.length self |> float_of_int)));
  ("at", Primitive (fun [List self; Number index] -> List.nth self (int_of_float index)));
  ("+", Primitive (fun [List self; List other] -> List (self @ other)));
  ("::", Primitive (fun [List self; other] -> List (other :: self)));
];
impl None (Type dict_type) [
  ("at", Primitive (fun [Dict self; index] -> Hashtbl.find self index));
  ("pairs", Primitive (fun [Dict self] -> List (Hashtbl.to_seq self |> List.of_seq |> List.map (fun (k, v) -> List [k; v])));)
];
impl (Some printable_trait) (Type function_type) [
  ("to_string", Primitive (function [Function (n, _, _, _)] -> String ("<function" ^ (if n = "" then "" else " " ^ n) ^ ">") | [Primitive _] -> String "<primitive>"));
];
impl (Some printable_trait) (Type type_type) [
  ("to_string", Primitive (fun [Type t] -> String ("<type " ^ t.name ^ ">")));
];
impl (Some printable_trait) (Type trait_type) [
  ("to_string", Primitive (fun [Trait t] -> String ("<trait " ^ t.name ^ ">")));
];
impl (Some printable_trait) (Trait error_trait) [
  ("to_string", Primitive (fun [Struct (t, fs)] ->
    let (String msg) = Hashtbl.find fs "msg" in
    String (t.name ^ ": " ^ msg)));
];