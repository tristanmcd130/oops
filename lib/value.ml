type t = {class': class'; repr: repr}
and class' = {name: string; super: class' option; meta: class'; methods: (string, t) Hashtbl.t}
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

let rec object_class = {name = "Object"; super = None; meta = object_metaclass; methods = Hashtbl.create 16}
and object_metaclass = {name = "ObjectMetaclass"; super = Some class_class; meta = class_class; methods = Hashtbl.create 16}
and class_class = {name = "Class"; super = Some object_class; meta = class_metaclass; methods = Hashtbl.create 16}
and class_metaclass = {name = "ClassMetaclass"; super = Some object_metaclass; meta = class_class; methods = Hashtbl.create 16}
let null_class = {name = "Null"; super = Some object_class; meta = {name = "NullMetaclass"; super = Some object_metaclass; meta = class_class; methods = Hashtbl.create 16}; methods = Hashtbl.create 16}
let bool_class = {name = "Bool"; super = Some object_class; meta = {name = "BoolMetaclass"; super = Some object_metaclass; meta = class_class; methods = Hashtbl.create 16}; methods = Hashtbl.create 16}
let number_class = {name = "Number"; super = Some object_class; meta = {name = "NumberMetaclass"; super = Some object_metaclass; meta = class_class; methods = Hashtbl.create 16}; methods = Hashtbl.create 16}
let string_class = {name = "String"; super = Some object_class; meta = {name = "StringMetaclass"; super = Some object_metaclass; meta = class_class; methods = Hashtbl.create 16}; methods = Hashtbl.create 16}
let list_class = {name = "List"; super = Some object_class; meta = {name = "ListMetaclass"; super = Some object_metaclass; meta = class_class; methods = Hashtbl.create 16}; methods = Hashtbl.create 16}
let function_class = {name = "Function"; super = Some object_class; meta = {name = "FunctionMetaclass"; super = Some object_metaclass; meta = class_class; methods = Hashtbl.create 16}; methods = Hashtbl.create 16}

let make_null () = {class' = null_class; repr = RNull}
let make_bool bool = {class' = bool_class; repr = RBool bool}
let make_number number = {class' = number_class; repr = RNumber number}
let make_string string = {class' = string_class; repr = RString string}
let make_list list = {class' = bool_class; repr = RList (Array.of_list list)}
let make_function name params body env = {class' = function_class; repr = RFunction (name, params, body, env)}
let make_primitive func = {class' = function_class; repr = RPrimitive func}

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
  | _ -> get_method obj name

let rec call func args eval =
  match func.repr with
  | RFunction (_, ps, b, e) -> eval b (Env.create (List.combine ps args) (Some e))
  | RPrimitive p -> p args
  | RClass _ -> call (dot func "new") [] eval
  | _ -> failwith "Not a function"

Hashtbl.replace object_metaclass.methods "new" (make_primitive (fun ({repr = RClass self} :: args) ->
  let obj = {class' = self; repr = RObject (Hashtbl.create 16)} in
  call (dot obj "init") args;
  obj
))