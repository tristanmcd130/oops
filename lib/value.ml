type t =
| VNull
| VBool of bool
| VNumber of float
| VString of string
| VList of t list
| VDict of (t, t) Hashtbl.t
| VFunction of string * string list * Exp.t * t Env.t
| VPrimitive of (t list -> t)
| VStruct of (type' * (string, t) Hashtbl.t)
| VType of type'
| VTrait of trait
and type' = {name: string; mutable traits: trait list; fields: string list; methods: (string, t) Hashtbl.t}
and trait = {name: string; abs_methods: string list; methods: (string, t) Hashtbl.t}

exception Runtime_error of t

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
let error_trait = {name = "Error"; abs_methods = ["message"]; methods = [
  ("to_string", VPrimitive (fun [VStruct self] ->
    let (VString msg) = Hashtbl.find self.fields "msg" in
    VString (self.type'.name ^ ": " ^ msg)));
] |> List.to_seq |> Hashtbl.of_seq}
let field_undefined_error_type = {name = "FieldUndefinedError"; traits = [error_trait]; fields = ["msg"]; methods = Hashtbl.create 16}
let trait_not_implemented_error_type = {name = "TraitNotImplementedError"; traits = [error_trait]; fields = ["msg"]; methods = Hashtbl.create 16}
let variable_undefined_error_type = {name = "VariableUndefinedError"; traits = [error_trait]; fields = ["msg"]; methods = Hashtbl.create 16}
let printable_trait = {name = "Printable"; abs_methods = ["to_string"]; methods = Hashtbl.create 0}
let equal_trait = {name = "Equal"; abs_methods = ["=="]; methods = [("!=", VFunction ("!=", ["other"], ECall(EDot (ECall (EDot (EVar "self", "=="), [EVar "other"]), "not"), []), Env.create [] None))] |> List.to_seq |> Hashtbl.of_seq}

let make_type name fields = VType {name = name; traits = [base_trait]; fields = fields; methods = Hashtbl.create 16}
let make_trait name abs_methods methods = VTrait {name = name; traits = []; abs_methods = abs_methods; methods = methods |> List.to_seq |> Hashtbl.of_seq}
let make_struct type' args = VStruct (type', List.combine type'.fields args |> List.to_seq |> Hashtbl.of_seq)
let throw error_type msg = raise (Runtime_error (make_struct error_type [VString msg]))

let type_of = function
| VStruct (t, _) -> t
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
    | None -> throw field_undefined_error_type (type'.name ^ " does not define method " ^ name)
let get_method value name = get_method_from_type (type_of value) name |> bind_self value
let dot value name =
  match value with
  | VStruct (_, fs) ->
    (match Hashtbl.find_opt fs name with
    | Some f -> f
    | None -> get_method obj name)
  | v -> get_method v name

let rec match' pattern value =
  match (pattern, value) with
  | (Exp.ENull, VNull) -> Some []
  | (EBool b, VBool b') when b = b' -> Some []
  | (ENumber n, VNumber n') when n = n' -> Some []
  | (EString s, VString s') when s = s' -> Some []
  | (EList [], VList []) -> Some []
  | (EList (p :: ps), VList (v :: vs)) ->
    (match match' p v with
    | Some bs ->
      (match match' (EList ps) (VList vs) with
      | Some bs' -> Some (bs' @ bs)
      | None -> None)
    | None -> None)
  (* TODO: add dict without eval *)
  | (EVar "_", _) -> Some []
  | (EVar n, v) -> Some [(n, v)]
  | (ECall (EDot (ps, "::"), [p]), VList (v :: vs)) ->
    (match match' p v with
    | Some bs ->
      (match match' ps (VList vs) with
      | Some bs' -> Some (bs' @ bs)
      | None -> None)
    | None -> None)
  | (ECall (EVar s, ps), VStruct {type' = t; fields = fs}) when s = t.name -> match' (EList ps) (VList (fs |> Hashtbl.to_seq_values |> List.of_seq))
  | (ECall (EDot (p1, "or"), [p2]), v) ->
    (match match' p1 v with
    | Some bs -> Some bs
    | None -> match' p2 v)
  | (ECall (EDot (p1, "and"), [p2]), v) ->
    (match match' p1 v with
    | Some bs ->
      (match match' p2 v with
      | Some bs' -> Some (bs' @ bs)
      | None -> None)
    | None -> None)
  | _ -> None

let rec match' pattern value =
  match (pattern, value) with
  | (Exp.ENull, VNull) -> Some []
  | (EBool b, VBool b') when b = b' -> Some []
  | (ENumber n, VNumber n') when n = n' -> Some []
  | (EString s, VString s') when s = s' -> Some []
  | (EList [], VList []) -> Some []
  | (EList (p :: ps), VList (v :: vs)) ->
    (match match' p v with
    | Some bs ->
      (match match' (EList ps) (VList vs) with
      | Some bs' -> Some (bs' @ bs)
      | None -> None)
    | None -> None)
  (* TODO: add dict *)
  | (EVar "_", _) -> Some []
  | (EVar n, v) -> Some [(n, v)]
  | (ECall (EDot (ps, "::"), [p]), VList (v :: vs)) ->
    (match match' p v with
    | Some bs ->
      (match match' ps (VList vs) with
      | Some bs' -> Some (bs' @ bs)
      | None -> None)
    | None -> None)
  | (ECall (EVar s, ps), VStruct (t, fs)) when s = t.name -> match' (EList ps) (VList (fs |> Hashtbl.to_seq_values |> List.of_seq))
  | (ECall (EDot (p1, "or"), [p2]), v) ->
    (match match' p1 v with
    | Some bs -> Some bs
    | None -> match' p2 v)
  | (ECall (EDot (p1, "and"), [p2]), v) ->
    (match match' p1 v with
    | Some bs ->
      (match match' p2 v with
      | Some bs' -> Some (bs' @ bs)
      | None -> None)
    | None -> None)
  | _ -> None

let rec match' pattern value =
  match (pattern, value) with
  | (Exp.ENull, VNull) -> Some []
  | (EBool b, VBool b') when b = b' -> Some []
  | (ENumber n, VNumber n') when n = n' -> Some []
  | (EString s, VString s') when s = s' -> Some []
  | (EList [], VList []) -> Some []
  | (EList (p :: ps), VList (v :: vs)) ->
    (match match' p v with
    | Some bs ->
      (match match' (EList ps) (VList vs) with
      | Some bs' -> Some (bs' @ bs)
      | None -> None)
    | None -> None)
  (* TODO: add dict *)
  | (EVar "_", _) -> Some []
  | (EVar n, v) -> Some [(n, v)]
  | (ECall (EDot (ps, "::"), [p]), VList (v :: vs)) ->
    (match match' p v with
    | Some bs ->
      (match match' ps (VList vs) with
      | Some bs' -> Some (bs' @ bs)
      | None -> None)
    | None -> None)
  | (ECall (EVar s, ps), VStruct (t, fs)) when s = t.name -> match' (EList ps) (VList (fs |> Hashtbl.to_seq_values |> List.of_seq))
  | (ECall (EDot (p1, "or"), [p2]), v) ->
    (match match' p1 v with
    | Some bs -> Some bs
    | None -> match' p2 v)
  | (ECall (EDot (p1, "and"), [p2]), v) ->
    (match match' p1 v with
    | Some bs ->
      (match match' p2 v with
      | Some bs' -> Some (bs' @ bs)
      | None -> None)
    | None -> None)
  | _ -> None

let subset a b = List.for_all (fun x -> List.mem x b) a
let method_names type' = (type'.methods |> Hashtbl.to_seq_keys |> List.of_seq) @ (type'.traits |> List.map (fun (x: trait) -> x.methods |> Hashtbl.to_seq_keys |> List.of_seq) |> List.concat)
let add_methods type' methods = methods |> List.to_seq |> Hashtbl.replace_seq type'.methods
let impl trait type' methods =
  match trait with
  | Some t ->
    let ms = method_names type' @ List.map fst methods in
    if subset t.abs_methods ms then
      (type'.traits <- t :: type'.traits;
      add_methods type' methods)
    else
      throw trait_not_implemented_error_type (t.name ^ " not fully implemented: " ^ String.concat ", " (List.filter (fun x -> not (List.mem x ms)) t.abs_methods))
  | None -> add_methods type' methods;;

impl (Some printable_trait) null_type [
  ("to_string", VPrimitive (fun _ -> VString "null"));
];
impl None bool_type [
  ("and", VPrimitive (fun [VBool self; VBool other] -> VBool (self && other)));
  ("or", VPrimitive (fun [VBool self; VBool other] -> VBool (self || other)));
  ("not", VPrimitive (fun [VBool self; VBool other] -> VBool (not self)));
];
impl (Some printable_trait) bool_type [
  ("to_string", VPrimitive (fun [VBool self ] -> VString (string_of_bool self)));
];
impl None number_type [
  ("+", VPrimitive (fun [VNumber self; VNumber other] -> VNumber (self +. other)));
  ("-", VPrimitive (fun [VNumber self; VNumber other] -> VNumber (self -. other)));
  ("u-", VPrimitive (fun [VNumber self] -> VNumber ~-.self));
  ("*", VPrimitive (fun [VNumber self; VNumber other] -> VNumber (self *. other)));
  ("/", VPrimitive (fun [VNumber self; VNumber other] -> VNumber (self /. other)));
  ("%", VPrimitive (fun [VNumber self; VNumber other] -> VNumber (mod_float self other)));
  ("<", VPrimitive (fun [VNumber self; VNumber other] -> VBool (self < other)));
  ("<=", VPrimitive (fun [VNumber self; VNumber other] -> VBool (self <= other)));
  ("!=", VPrimitive (fun [VNumber self; VNumber other] -> VBool (self <> other)));
  (">", VPrimitive (fun [VNumber self; VNumber other] -> VBool (self > other)));
  (">=", VPrimitive (fun [VNumber self; VNumber other] -> VBool (self >= other)));
];
impl (Some equal_trait) number_type [
  ("==", VPrimitive (fun [VNumber self; VNumber other] -> VBool (self = other)));
];
impl (Some printable_trait) number_type [
  ("to_string", VPrimitive (fun [VNumber self ] -> VString (Printf.sprintf "%g" self)));
];
impl None string_type [
  ("to_string", VPrimitive (fun [self] -> self));
  ("+", VPrimitive (fun [VString self; VString other] -> VString (String.cat self other)));
  ("head", VPrimitive (fun [VString self] ->  VString (self.[0] |> String.make 1)));
  ("tail", VPrimitive (fun [VString self] ->  VString (String.sub self 1 (String.length self - 1))));
  ("length", VPrimitive (fun [VString self] ->  VNumber (String.length self |> float_of_int)));
];
impl (Some printable_trait) string_type [
  ("to_string", VPrimitive (fun [self] -> self));
];
impl None list_type [
  ("head", VPrimitive (fun [VList self] ->  List.hd self));
  ("tail", VPrimitive (fun [VList self] ->  VList (List.tl self)));
  ("length", VPrimitive (fun [VList self] ->  VNumber (List.length self |> float_of_int)));
  ("at", VPrimitive (fun [VList self; VNumber index] -> List.nth self (int_of_float index)));
  ("+", VPrimitive (fun [VList self; VList other] -> VList (self @ other)));
  ("::", VPrimitive (fun [VList self; other] -> VList (other :: self)));
];
impl None dict_type [
  ("at", VPrimitive (fun [VDict self; index] -> Hashtbl.find self index));
  ("pairs", VPrimitive (fun [VDict self] -> VList (Hashtbl.to_seq self |> List.of_seq |> List.map (fun (k, v) -> VList [k; v])));)
];
impl (Some printable_trait) function_type [
  ("to_string", VPrimitive (function [VFunction (n, _, _, _)] -> VString ("<function" ^ (if n = "" then "" else " " ^ n) ^ ">") | [VPrimitive _] -> VString "<primitive>"));
];
impl (Some printable_trait) type_type [
  ("to_string", VPrimitive (fun [VType t] -> VString ("<type " ^ t.name ^ ">")));
];
impl (Some printable_trait) trait_type [
  ("to_string", VPrimitive (fun [VTrait t] -> VString ("<trait " ^ t.name ^ ">")));
];
impl (Some printable_trait) (VTrait error_trait) [
  ("to_string", VPrimitive (fun [VStruct (t, fs)] ->
    let (VString msg) = Hashtbl.find fs "msg" in
    VString (t.name ^ ": " ^ msg)));
];