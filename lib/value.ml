type t =
| VNull
| VBool of bool
| VNumber of float
| VString of string
| VList of t list
| VDict of (t, t) Hashtbl.t
| VFunction of string * string list * Exp.t * t Env.t
| VPrimitive of (t list -> t)
| VObject of {class': class'; fields: (string, t) Hashtbl.t}
| VClass of class'
| VTrait of trait
and class' = {name: string; super: class' option; trait: trait; methods: (string, t) Hashtbl.t}
and trait = {name: string; abs_methods: string list; methods: (string, t) Hashtbl.t}

let empty_trait = {name = "Empty"; abs_methods = []; methods = Hashtbl.create 0}
let object_class = {name = "Object"; super = None; trait = empty_trait; methods = Hashtbl.create 16}
let null_class = {name = "Null"; super = Some object_class; trait = empty_trait; methods = Hashtbl.create 16}
let bool_class = {name = "Bool"; super = Some object_class; trait = empty_trait; methods = Hashtbl.create 16}
let number_class = {name = "Number"; super = Some object_class; trait = empty_trait; methods = Hashtbl.create 16}
let string_class = {name = "String"; super = Some object_class; trait = empty_trait; methods = Hashtbl.create 16}
let list_class = {name = "List"; super = Some object_class; trait = empty_trait; methods = Hashtbl.create 16}
let dict_class = {name = "Dict"; super = Some object_class; trait = empty_trait; methods = Hashtbl.create 16}
let function_class = {name = "Function"; super = Some object_class; trait = empty_trait; methods = Hashtbl.create 16}
let class_class = {name = "Class"; super = Some object_class; trait = empty_trait; methods = Hashtbl.create 16}
let trait_class = {name = "Trait"; super = Some object_class; trait = empty_trait; methods = Hashtbl.create 16}

let override (trait1: trait) (trait2: trait) =
  let new_methods = trait1.methods in
  Hashtbl.replace_seq new_methods (trait2.methods |> Hashtbl.to_seq);
  {name = trait1.name ^ " + " ^ trait2.name; abs_methods = trait1.abs_methods @ trait2.abs_methods; methods = new_methods}
let symmetric_sum (trait1: trait) (trait2: trait) =
  if List.exists (fun x -> Hashtbl.mem trait2.methods x) (trait1.methods |> Hashtbl.to_seq_keys |> List.of_seq) || List.exists (fun x -> Hashtbl.mem trait1.methods x) (trait2.methods |> Hashtbl.to_seq_keys |> List.of_seq) then
    failwith "Cannot sum traits: methods overlap";
  if List.exists (fun x -> List.mem x trait2.abs_methods) trait1.abs_methods || List.exists (fun x -> List.mem x trait1.abs_methods) trait2.abs_methods then
    failwith "Cannot sum traits: abstract methods overlap";
  override trait1 trait2
let alias (trait: trait) name new_name =
  let new_methods = trait.methods in
  Hashtbl.replace new_methods new_name (Hashtbl.find new_methods name);
  {name = trait.name ^ "[" ^ new_name ^ " -> " ^ name ^ "]"; abs_methods = trait.abs_methods; methods = new_methods}
let exclude (trait1: trait) (trait2: trait) =
  let new_methods = trait1.methods in
  Hashtbl.filter_map_inplace (fun k v -> if List.mem k (trait2.methods |> Hashtbl.to_seq_keys |> List.of_seq) then None else Some v) new_methods;
  {name = trait1.name ^ " - " ^ trait2.name; abs_methods = List.filter (fun x -> not (List.mem x trait2.abs_methods)) trait1.abs_methods; methods = new_methods}

let rec get_method_names class' =
  (class'.methods |> Hashtbl.to_seq_keys |> List.of_seq) @ (class'.trait.methods |> Hashtbl.to_seq_keys |> List.of_seq) @ (match class'.super with Some c -> get_method_names c | None -> [])
let rec subset a b = List.for_all (fun x -> List.mem x b) a
let uses_trait class' trait =
  subset trait.abs_methods (get_method_names class' @ (trait.methods |> Hashtbl.to_seq_keys |> List.of_seq))
let make_class name super trait methods =
  VClass (let c = {
    name = name;
    super = Some (match super with Some (VClass c) -> c | None -> object_class);
    trait = (match trait with Some (VTrait t) -> t | None -> empty_trait);
    methods = methods |> List.to_seq |> Hashtbl.of_seq
  } in match trait with
  | Some (VTrait t) -> if uses_trait c t then c else failwith "Class doesn't fully implement trait"
  | None -> c)
let make_trait name super abs_methods methods = VTrait (symmetric_sum (match super with Some (VTrait t) -> t | None -> empty_trait) {name = name; abs_methods = abs_methods; methods = methods |> List.to_seq |> Hashtbl.of_seq})

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
| VFunction (n, ps, b, e) -> VFunction (n, ps, b, Env.create [("self", obj)] (Some e))
| VPrimitive p -> VPrimitive (fun args -> p (obj :: args))
| _ -> failwith "Not a function"
let bind_super class' = function
| VFunction (n, ps, b, e) ->
  VFunction (n, ps, b, match class' with
  | Some c -> Env.create [("super", VClass c)] (Some e)
  | None -> e)
| VPrimitive p -> VPrimitive p
| _ -> failwith "Not a function"
let rec get_method_from_class class' name =
  match Hashtbl.find_opt class'.methods name with
  | Some m -> m |> bind_super class'.super
  | None ->
    match Hashtbl.find_opt class'.trait.methods name with
    | Some m -> m |> bind_super class'.super
    | None ->
      match class'.super with
      | Some c -> get_method_from_class c name
      | None -> failwith ("Class " ^ class'.name ^ " does not define method " ^ name)
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
add_methods object_class [
  ("init", fun _ -> VNull);
  ("to_string", fun _ -> VString "<object>");
  ("==", fun [self; other] -> VBool (self = other))
];
add_methods null_class [
  ("to_string", fun _ -> VString "null");
  ("to_bool", fun _ -> VBool false);
];
add_methods bool_class [
  ("to_string", fun [VBool self ] -> VString (string_of_bool self));
  ("to_bool", fun [self] -> self);
  ("and", fun [VBool self; VBool other] -> VBool (self && other));
  ("or", fun [VBool self; VBool other] -> VBool (self || other));
  ("not", fun [VBool self; VBool other] -> VBool (not self));
];
add_methods number_class [
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
add_methods string_class [
  ("to_string", fun [self] -> self);
  ("to_bool", fun [VString self] -> VBool (self <> ""));
  ("+", fun [VString self; VString other] -> VString (String.cat self other));
];
add_methods list_class [
  ("to_bool", fun [VList self] -> VBool (self <> []));
  ("head", fun [VList self] ->  List.hd self);
  ("tail", fun [VList self] ->  VList (List.tl self));
  ("at", fun [VList self; VNumber index] -> List.nth self (int_of_float index));
  ("+", fun [VList self; VList other] -> VList (self @ other));
];
add_methods dict_class [
  ("to_bool", fun [VDict self] -> VBool (Hashtbl.length self > 0));
  ("at", fun [VDict self; index] -> Hashtbl.find self index);
];
add_methods function_class [
  ("to_string", function [VFunction (n, _, _, _)] -> VString ("<function " ^ n ^ ">") | [VPrimitive self] -> VString "<primitive>");
];
add_methods class_class [
  ("to_string", fun [VClass self] -> VString ("<class " ^ self.name ^ ">"));
];
add_methods trait_class [
  ("to_string", fun [VTrait self] -> VString ("<trait " ^ self.name ^ ">"));
  ("+", fun [VTrait self; VTrait other] -> VTrait (symmetric_sum self other));
  ("override", fun [VTrait self; VTrait other] -> VTrait (override self other));
  ("alias", fun [VTrait self; VString name; VString new_name] -> VTrait (alias self name new_name));
  ("-", fun [VTrait self; VTrait other] -> VTrait (exclude self other));
];