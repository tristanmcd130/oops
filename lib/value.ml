type t = {class': class'; repr: repr}
and class' = {name: string; super: class' option; meta: class'; fields: (string, t) Hashtbl.t; methods: (string, t) Hashtbl.t}
and repr =
| RNull
| RBool of bool
| RNumber of float
| RString of string
| RList of t array
| RFunction of string * string list * Exp.t * t Env.t
| RPrimitive of (t list -> t)
| RObject of (string, t) Hashtbl.t
| RClass of class'

let rec object_class = {name = "Object"; super = None; meta = object_metaclass; fields = Hashtbl.create 0; methods = Hashtbl.create 0}
and object_metaclass = {name = "ObjectMetaclass"; super = Some class_class; meta = class_class; fields = Hashtbl.create 0; methods = Hashtbl.create 0}
and class_class = {name = "Class"; super = Some object_class; meta = class_metaclass; fields = Hashtbl.create 0; methods = Hashtbl.create 0}
and class_metaclass = {name = "ClassMetaclass"; super = Some object_metaclass; meta = class_class; fields = Hashtbl.create 0; methods = Hashtbl.create 0}
let make_class name super methods static_fields static_methods =
  let super' = match super with Some s -> s | None -> object_class in
  {name = name; super = Some super'; meta = {name = name ^ "Metaclass"; super = Some super'.meta; meta = class_class; fields = Hashtbl.create 0; methods = static_methods |> List.to_seq |> Hashtbl.of_seq}; fields = static_fields |> List.to_seq |> Hashtbl.of_seq; methods = methods |> List.to_seq |> Hashtbl.of_seq}
let null_class = make_class "Null" (Some object_class) [] [] []
let bool_class = make_class "Bool" (Some object_class) [] [] []
let number_class = make_class "Number" (Some object_class) [] [] []
let string_class = make_class "String" (Some object_class) [] [] []
let list_class = make_class "List" (Some object_class) [] [] []
let function_class = make_class "Function" (Some object_class) [] [] []
let make_null () = {class' = null_class; repr = RNull}
let make_bool bool = {class' = bool_class; repr = RBool bool}
let make_number number = {class' = number_class; repr = RNumber number}
let make_string string = {class' = string_class; repr = RString string}
let make_list list = {class' = bool_class; repr = RList (Array.of_list list)}
let make_function name params body env = {class' = function_class; repr = RFunction (name, params, body, env)}
let make_primitive func = {class' = function_class; repr = RPrimitive func}
let of_class class' = {class' = class'.meta; repr = RClass class'}
let to_class value =
  match value.repr with
  | RClass c -> c
  | _ -> failwith "Not a class"

let rec get_method_from_class class' name =
  match Hashtbl.find_opt class'.methods name with
  | Some m -> m
  | None ->
    (match class'.super with
    | Some c -> get_method_from_class c name
    | None -> failwith ("Method " ^ name ^ " not found"))
(* TODO: BIND SUPER AFTER GETTING METHOD FROM CLASS *)
let get_method obj name =
  let m = get_method_from_class obj.class' name in
  match m.repr with
  | RFunction (n, ps, b, e) -> {m with repr = RFunction (n, ps, b, Env.create [("self", obj)] (Some e))}
  | RPrimitive p -> {m with repr = RPrimitive (fun args -> p (obj :: args))}
  | _ -> failwith "Not a method"
let dot obj name =
  match obj.repr with
  | RObject o ->
    (match Hashtbl.find_opt o name with
    | Some f -> f
    | None -> get_method obj name)
  | RClass c ->
    (match Hashtbl.find_opt c.fields name with
    | Some f -> f
    | None -> get_method obj name)
  | _ -> get_method obj name

let add_methods class' methods = methods |> List.map (fun (n, v) -> (n, make_primitive v)) |> List.to_seq |> Hashtbl.replace_seq class'.methods;;
add_methods object_class [
  ("to_string", fun _ -> "<object>" |> make_string);
];
add_methods null_class [
  ("to_string", fun _ -> make_string "null");
  ("to_bool", fun _ -> make_bool false);
];
add_methods bool_class [
  ("to_string", fun ({repr = RBool b} :: _) -> b |> string_of_bool |> make_string);
  ("to_bool", fun (self :: _) -> self);
];
add_methods number_class [
  ("to_string", fun ({repr = RNumber n} :: _) -> n |> string_of_float |> make_string);
  ("to_bool", fun ({repr = RNumber n} :: _) -> make_bool (n = 0.0));
];
add_methods string_class [
  ("to_string", fun (self :: _) -> self);
  ("to_bool", fun ({repr = RString s} :: _) -> make_bool (s = ""));
];