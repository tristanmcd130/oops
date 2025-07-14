include Types

type t = Types.value

exception Runtime_error of t

let rec to_string = function
| Bool b -> string_of_bool b
| Number n -> Printf.sprintf "%g" n
| String s -> s
| List l -> "[" ^ (List.map to_string l |> String.concat ", ") ^ "]"
| Map m -> "{" ^ (m |> Hashtbl.to_seq |> List.of_seq |> List.map (fun (k, v) -> to_string k ^ ": " ^ to_string v) |> String.concat ", ") ^ "}"
| Closure {name = ""} -> "<anonymous function>"
| Closure {name} -> "<function " ^ name ^ ">"
| Cell c -> "<cell containing " ^ to_string !c ^ ">"
| Primitive _ -> "<primitive>"
| Struct (t, fs) -> t.name ^ "(" ^ (Array.map to_string fs |> Array.to_list |> String.concat ", ") ^ ")"
| Type t -> "<type " ^ t.name ^ ">"
| Trait t -> "<trait " ^ t.name ^ ">"
| Method (_, c) -> to_string (Closure c)
| Module m -> "<module from " ^ m.filename ^ ">"
let base_trait = {name = "Base"; requires = []; provides = [
  ("==", Primitive (fun [self; other] -> Bool (self = other)));
  ("!=", Primitive (fun [self; other] -> Bool (self <> other)));
  ("to_string", Primitive (fun [self] -> String (to_string self)));
] |> List.to_seq |> Hashtbl.of_seq; traits = []}
let bool_type = {name = "Bool"; fields = Hashtbl.create 0; methods = [
  ("not", Primitive (fun [Bool self] -> Bool (not self)));
  ("and", Primitive (fun [Bool self; Bool other] -> Bool (self && other)));
  ("or", Primitive (fun [Bool self; Bool other] -> Bool (self || other)));
] |> List.to_seq |> Hashtbl.of_seq; traits = [base_trait]}
let number_type = {name = "Number"; fields = Hashtbl.create 0; methods = [
  ("u-", Primitive (fun [Number self] -> Number (-.self)));
  ("+", Primitive (fun [Number self; Number other] -> Number (self +. other)));
  ("-", Primitive (fun [Number self; Number other] -> Number (self -. other)));
  ("*", Primitive (fun [Number self; Number other] -> Number (self *. other)));
  ("/", Primitive (fun [Number self; Number other] -> Number (self /. other)));
  ("%", Primitive (fun [Number self; Number other] -> Number (mod_float self other)));
  ("<", Primitive (fun [Number self; Number other] -> Bool (self < other)));
  ("<=", Primitive (fun [Number self; Number other] -> Bool (self <= other)));
  (">", Primitive (fun [Number self; Number other] -> Bool (self > other)));
  (">=", Primitive (fun [Number self; Number other] -> Bool (self >= other)));
] |> List.to_seq |> Hashtbl.of_seq; traits = [base_trait]}
let string_type = {name = "String"; fields = Hashtbl.create 0; methods = [
  ("+", Primitive (fun [String self; String other] -> String (self ^ other)));
] |> List.to_seq |> Hashtbl.of_seq; traits = [base_trait]}
let list_type = {name = "List"; fields = Hashtbl.create 0; methods = [
  ("::", Primitive (fun [List self; other] -> List (other :: self)));
  ("head", Primitive (fun [List (h :: _)] -> h));
  ("tail", Primitive (fun [List (_ :: t)] -> List t));
] |> List.to_seq |> Hashtbl.of_seq; traits = [base_trait]}
let map_type = {name = "Map"; fields = Hashtbl.create 0; methods = [] |> List.to_seq |> Hashtbl.of_seq; traits = [base_trait]}
let function_type = {name = "Function"; fields = Hashtbl.create 0; methods = [] |> List.to_seq |> Hashtbl.of_seq; traits = [base_trait]}
let type_type = {name = "Type"; fields = Hashtbl.create 0; methods = [] |> List.to_seq |> Hashtbl.of_seq; traits = [base_trait]}
let trait_type = {name = "Trait"; fields = Hashtbl.create 0; methods = [] |> List.to_seq |> Hashtbl.of_seq; traits = [base_trait]}
let module_type = {name = "Module"; fields = Hashtbl.create 0; methods = [] |> List.to_seq |> Hashtbl.of_seq; traits = [base_trait]}
let rec type_of = function
| Bool _ -> bool_type
| Number _ -> number_type
| String _ -> string_type
| List _ -> list_type
| Map _ -> map_type
| Closure _ | Primitive _ | Method _ -> function_type
| Cell c -> type_of !c
| Struct (t, _) -> t
| Type _ -> type_type
| Trait _ -> trait_type
| Module _ -> module_type
let bind_self self = function
| Closure c -> Method (self, c)
| Primitive p -> Primitive (fun args -> p (self :: args))
| x -> failwith ("Cannot bind self in " ^ to_string x)
let rec get_method_from_traits name = function
| [] -> None
| t :: ts ->
  match Hashtbl.find_opt t.provides name with
  | None ->
    (match get_method_from_traits name t.traits with
    | None -> get_method_from_traits name ts
    | m -> m)
  | m -> m
let get_method obj name =
  (match Hashtbl.find_opt (type_of obj).methods name with
  | None ->
    (match get_method_from_traits name (type_of obj).traits with
    | None -> failwith (to_string (Type (type_of obj)) ^ " has no field/method " ^ name)
    | Some m -> m)
  | Some m -> m) |> bind_self obj
let get_field obj name =
  match obj with
  | Struct (t, fs) ->
    (match Hashtbl.find_opt t.fields name with
    | None -> get_method obj name
    | Some i -> fs.(i))
  | Module m ->
    if List.mem name m.exports then
      Hashtbl.find m.vars name
    else
      failwith (to_string (Module m) ^ " does not export " ^ name)
  | _ -> get_method obj name
let set_field obj name value =
  match obj with
  | Struct (t, fs) ->
    (match Hashtbl.find_opt t.fields name with
    | None -> failwith (to_string (Type t) ^ " has no field " ^ name)
    | Some i -> Array.set fs i value)
  | Module m ->
    if List.mem name m.exports then
      Hashtbl.replace m.vars name value
    else
      failwith (to_string (Module m) ^ " does not export " ^ name)
  | _ -> failwith (to_string obj ^ " does not have fields")
let subset a b = List.for_all (fun x -> List.mem x b) a
let impl trait = function
| Type ty ->
  if not (List.mem trait ty.traits) then
    (if subset trait.requires (ty.methods |> Hashtbl.to_seq_keys |> List.of_seq) then
      ty.traits <- trait :: ty.traits
    else
      failwith (to_string (Type ty) ^ " does not fully implement " ^ to_string (Trait trait) ^ ": missing " ^ String.concat ", " (List.filter (fun x -> not (ty.methods |> Hashtbl.to_seq_keys |> List.of_seq |> List.mem x)) trait.requires)))
| Trait t ->
  if not (List.mem trait t.traits) then
    (if subset trait.requires (t.provides |> Hashtbl.to_seq_keys |> List.of_seq) then
      t.traits <- trait :: t.traits
    else
      failwith (to_string (Trait t) ^ " does not fully implement " ^ to_string (Trait trait) ^ ": missing " ^ String.concat ", " (List.filter (fun x -> not (t.provides |> Hashtbl.to_seq_keys |> List.of_seq |> List.mem x)) trait.requires)))
| x -> failwith ("Cannot implement for " ^ to_string x ^ ": it is not a trait or a type")