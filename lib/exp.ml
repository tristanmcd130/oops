module rec Exp: sig
	type t =
	| EObject of object'
  | EBool of bool
  | ENumber of float
  | EString of string
  | EList of t list
  | EDict of (t, t) Hashtbl.t
	| EFunction of function'
  | EPrimitive of (t list -> t)
	| EClass of class'
	| ETrait of trait
	and object'
	and class'
  and trait
	and function'
  val make_null: unit -> t
	val make_bool: bool -> t
	val make_number: float -> t
	val make_string: string -> t
	val make_list: t list -> t
	val make_dict: (t * t) list -> t
	val make_function: string -> string list -> Ast.t -> Env.t -> function'
	val make_primitive: (t list -> t) -> t
  val make_class: string -> class' option -> trait list -> (string * t) list -> class'
  val make_trait: string -> string list -> (string * t) list -> trait
  val call_function: function' -> t list -> (Ast.t -> Env.t -> t) -> t
  val add_methods: class' -> (string * t) list -> unit
  val add_field: t -> string -> t -> unit
  val dot: t -> ?start_class: class' -> string -> t
  val class_of: t -> class'
  val superclass_of: t -> class'
  val to_class: t -> class'
  val to_trait: t -> trait
  val class_name: class' -> string
  val field_values: t -> t list
  val object_class: class'
  val class_class: class'
  val null_class: class'
  val number_class: class'
  val string_class: class'
  val list_class: class'
  val dict_class: class'
  val function_class: class'
  val primitive_class: class'
  val trait_class: class'
end = struct
  type t =
  | EObject of object'
  | EBool of bool
  | ENumber of float
  | EString of string
  | EList of t list
  | EDict of (t, t) Hashtbl.t
	| EFunction of function'
  | EPrimitive of (t list -> t)
	| EClass of class'
	| ETrait of trait
  and object' = {class': class'; fields: (string, t) Hashtbl.t}
  and class' = {name: string; superclass: class' option; mutable traits: trait list; methods: (string, t) Hashtbl.t}
  and trait = {name: string; abs_methods: string list; methods: (string, t) Hashtbl.t}
  and function' = {name: string; params: string list; body: Ast.t; env: Env.t}

  let rec object_class = {name = "Object"; superclass = None; traits = []; methods = Hashtbl.create 16}
  and class_class = {name = "Class"; superclass = Some object_class; traits = []; methods = Hashtbl.create 16}
  and null_class = {name = "Null"; superclass = Some object_class; traits = []; methods = Hashtbl.create 16}
  and bool_class = {name = "Bool"; superclass = Some object_class; traits = []; methods = Hashtbl.create 16}
  and number_class = {name = "Number"; superclass = Some object_class; traits = []; methods = Hashtbl.create 16}
  and string_class = {name = "String"; superclass = Some object_class; traits = []; methods = Hashtbl.create 16}
  and list_class = {name = "List"; superclass = Some object_class; traits = []; methods = Hashtbl.create 16}
  and dict_class = {name = "Dict"; superclass = Some object_class; traits = []; methods = Hashtbl.create 16}
  and function_class = {name = "Function"; superclass = Some object_class; traits = []; methods = Hashtbl.create 16}
  and primitive_class = {name = "Primitive"; superclass = Some object_class; traits = []; methods = Hashtbl.create 16}
  and trait_class = {name = "Trait"; superclass = Some object_class; traits = []; methods = Hashtbl.create 16}
  let make_null () = EObject {class' = null_class; fields = Hashtbl.create 16}
  let make_bool (bool: bool) = EBool bool
  let make_number (number: float) = ENumber number
  let make_string (string: string) = EString string
  let make_list (list: t list) = EList list
  let make_dict (dict: (t * t) list) = EDict (Hashtbl.of_seq (List.to_seq dict))
  let make_function (name: string) (params: string list) (body: Ast.t) (env: Env.t) = {name = name; params = params; body = body; env = env}
  let make_primitive (func: t list -> t) = EPrimitive func
  let rec subset (list1: 'a list) (list2: 'a list): bool =
    match list1 with
    | [] -> true
    | x :: xs -> List.mem x list2 && subset xs list2
  let meets_trait_reqs (trait: trait) (methods: (string * t) list): bool =
    subset trait.abs_methods (List.map (fun (n, _) -> n) methods)
  let make_class (name: string) (superclass: class' option) (traits: trait list) (methods: (string * t) list): class' =
    assert (List.for_all (fun x -> meets_trait_reqs x methods) traits);
    {name = name; superclass = superclass; traits = traits; methods = Hashtbl.of_seq (List.to_seq methods)}
  let make_trait (name: string) (abs_methods: string list) (methods: (string * t) list): trait = {name = name; abs_methods = abs_methods; methods = Hashtbl.of_seq (List.to_seq methods)}
  let bool_object = {class' = bool_class; fields = Hashtbl.create 16}
  let number_object = {class' = number_class; fields = Hashtbl.create 16}
  let string_object = {class' = string_class; fields = Hashtbl.create 16}
  let list_object = {class' = list_class; fields = Hashtbl.create 16}
  let dict_object = {class' = dict_class; fields = Hashtbl.create 16}
  let function_object = {class' = function_class; fields = Hashtbl.create 16}
  let primitive_object = {class' = primitive_class; fields = Hashtbl.create 16}
  let class_object = {class' = class_class; fields = Hashtbl.create 16}
  let trait_object = {class' = trait_class; fields = Hashtbl.create 16}
  let object_of = function
  | EObject o -> o
	| EBool b -> bool_object
  | ENumber n -> number_object
  | EString s -> string_object
  | EList l -> list_object
  | EDict d -> dict_object
	| EFunction f -> function_object
  | EPrimitive p -> primitive_object
	| EClass c -> class_object
	| ETrait t -> trait_object
  let class_of (exp: t): class' = (object_of exp).class'
  let to_class = function
  | EClass c -> c
  | _ -> failwith "Not a class"
  let call_function (func: function') (args: t list) (eval: Ast.t -> Env.t -> t) = eval func.body (Env.make (List.combine func.params args) (Some func.env))
  let superclass_of (exp: t) =
    match (class_of exp).superclass with
    | Some s -> s
    | None -> failwith "No superclass"
  let rec get_method_from_traits (class': class') (name: string): t =
    (* print_endline ("GET METHOD FROM TRAIT: " ^ name); *)
    let rec helper (traits: trait list) (name: string): t =
      match traits with
      | [] -> (match class'.superclass with
        | Some c -> get_method_from_class c name
        | None -> failwith ("No method " ^ name))
      | t :: ts -> (match Hashtbl.find_opt t.methods name with
        | Some m -> bind_super m class'.superclass
        | None -> helper ts name)
    in helper class'.traits name
  and bind_super (func: t) (superclass: class' option): t =
    match superclass with
    | Some s -> (match func with
      | EFunction f -> EFunction (make_function f.name f.params f.body (Env.make [("super", EClass s)] (Some f.env)))
      | x -> x)
    | None -> func
  and get_method_from_class (class': class') (name: string): t =
    (* print_endline ("GET METHOD FROM CLASS: " ^ name); *)
    match Hashtbl.find_opt class'.methods name with
    | Some m -> bind_super m class'.superclass
    | None -> get_method_from_traits class' name
  let get_method (exp: t) (start_class: class') (name: string): t =
    (* print_endline ("GET METHOD: " ^ name); *)
    match get_method_from_class start_class name with
    | EFunction f -> EFunction (make_function f.name f.params f.body (Env.make [("self", exp)] (Some f.env)))
    | EPrimitive p -> EPrimitive (fun args -> p (exp :: args))
    | _ -> failwith "Not a method"
  let add_methods (class': class') (methods: (string * t) list) =
    Hashtbl.replace_seq class'.methods (List.to_seq methods)
  let dot (exp: t) ?(start_class = class_of exp) (name: string): t =
    match Hashtbl.find_opt (object_of exp).fields name with
    | Some f -> f
    | None -> get_method exp start_class name
  let add_field (exp: t) (field: string) (value: t) =
    Hashtbl.replace (object_of exp).fields field value
  let to_trait = function
  | ETrait t -> t
  | _ -> failwith "Not a trait"
  let class_name (class': class') = class'.name
  let field_values (exp: t) = (object_of exp).fields |> Hashtbl.to_seq_values |> List.of_seq;;

  add_methods object_class [
    ("init", make_primitive (fun _ -> make_null ()));
    ("to_string", make_primitive (fun [self] -> make_string ("<" ^ (class_of self).name ^ " instance>")));
    ("to_bool", make_primitive (fun _ -> make_bool true));
    ("==", make_primitive (fun [self; other] -> make_bool (Hashtbl.hash self = Hashtbl.hash other)));
    ("!", make_primitive (fun [self; other] -> make_bool (Hashtbl.hash self <> Hashtbl.hash other)));
  ];
  add_methods class_class [
    ("new", make_primitive (fun (EClass self :: args) -> EObject {class' = self; fields = Hashtbl.create 16}));
    ("to_string", make_primitive (fun [EClass self] -> make_string ("<" ^ self.name ^ " class>")));
    ("use", make_primitive (fun [EClass self; ETrait trait] -> assert (meets_trait_reqs trait (self.methods |> Hashtbl.to_seq |> List.of_seq)); self.traits <- trait :: self.traits; ETrait trait));
  ];
  add_methods null_class [
    ("to_string", make_primitive (fun _ -> make_string "null"));
  ];
  add_methods bool_class [
    ("to_string", make_primitive (fun [EBool self] -> make_string (string_of_bool self)));
    ("to_bool", make_primitive (fun [self] -> self));
    ("not", make_primitive (fun [EBool self] -> make_bool (not self)));
  ];
  add_methods number_class [
    ("+", make_primitive (fun [ENumber n1; ENumber n2] -> make_number (n1 +. n2)));
    ("-", make_primitive (fun [ENumber n1; ENumber n2] -> make_number (n1 -. n2)));
    ("*", make_primitive (fun [ENumber n1; ENumber n2] -> make_number (n1 *. n2)));
    ("/", make_primitive (fun [ENumber n1; ENumber n2] -> make_number (n1 /. n2)));
    ("%", make_primitive (fun [ENumber n1; ENumber n2] -> make_number (mod_float n1 n2)));
    ("<", make_primitive (fun [ENumber n1; ENumber n2] -> make_bool (n1 < n2)));
    ("<=", make_primitive (fun [ENumber n1; ENumber n2] -> make_bool (n1 <= n2)));
    (">", make_primitive (fun [ENumber n1; ENumber n2] -> make_bool (n1 > n2)));
    (">=", make_primitive (fun [ENumber n1; ENumber n2] -> make_bool (n1 >= n2)));
    ("to_string", make_primitive (fun [ENumber self] -> make_string (Printf.sprintf "%g" self)));
  ];
  add_methods string_class [
    ("to_string", make_primitive (fun [self] -> self));
  ];
  add_methods list_class [
    ("::", make_primitive (fun [EList self; other] -> make_list (other :: self)));
    ("get", make_primitive (fun [EList (x :: xs)] -> x));
    ("next", make_primitive (fun [EList (x :: xs)] -> EList xs));
    ("done", make_primitive (fun [EList l] -> EBool (List.length l = 0)));
  ];
  add_methods function_class [
    ("to_string", make_primitive (fun [EFunction self] -> make_string ("<function" ^ self.name ^ ">")));
  ];
  add_methods primitive_class [
    ("to_string", make_primitive (fun [EPrimitive self] -> make_string "<primitive>"));
  ];
  add_methods trait_class [
    ("to_string", make_primitive (fun [ETrait self] -> make_string ("<" ^ self.name ^ " trait>")));
  ];
end
and Env: sig
	type t
	val find: t -> string -> Exp.t
  val add: t -> string -> Exp.t -> unit
  val make: (string * Exp.t) list -> t option -> t
end = struct
  type t = {bindings: (string, Exp.t) Hashtbl.t; parent: t option}
  let rec find (env: t) (name: string): Exp.t =
    match Hashtbl.find_opt env.bindings name with
    | Some v -> v
    | None -> (match env.parent with
      | Some p -> find p name
      | None -> failwith (name ^ " not defined"))
  let add (env: t) (name: string) (value: Exp.t) =
    Hashtbl.replace env.bindings name value
  let make (bindings: (string * Exp.t) list) (parent: t option): t =
    {bindings = Hashtbl.of_seq (List.to_seq bindings); parent = parent}
end