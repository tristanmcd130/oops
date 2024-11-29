module rec Exp: sig
	type t =
	| EObject of object'
  | EBool of bool * object'
  | ENumber of float * object'
  | EString of string * object'
  | EList of t list * object'
  | EDict of (t, t) Hashtbl.t * object'
	| EFunction of function'
  | EPrimitive of (t list -> t) * object'
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
  val get_method: t -> ?start_class: class' -> string -> t
  val add_method: class' -> string -> t -> unit
  val add_field: t -> string -> t -> unit
  val dot: t -> string -> t
  val object_of: t -> object'
  val class_of: t -> class'
  val superclass_of: t -> class'
  val to_class: t -> class'
  val to_trait: t -> trait
  val object_class: class'
  val class_class: class'
  val list_class: class'
  val dict_class: class'
end = struct
  type t =
  | EObject of object'
  | EBool of bool * object'
  | ENumber of float * object'
  | EString of string * object'
  | EList of t list * object'
  | EDict of (t, t) Hashtbl.t * object'
	| EFunction of function'
  | EPrimitive of (t list -> t) * object'
	| EClass of class'
	| ETrait of trait
  and object' = {class': class'; fields: (string, t) Hashtbl.t}
  and class' = {object': object'; name: string; superclass: class' option; traits: trait list; methods: (string, t) Hashtbl.t}
  and trait = {object': object'; name: string; abs_methods: string list; methods: (string, t) Hashtbl.t}
  and function' = {object': object'; name: string; params: string list; body: Ast.t; env: Env.t}

  let rec object_class = {object' = {class' = class_class; fields = Hashtbl.create 16}; name = "Object"; superclass = None; traits = []; methods = Hashtbl.create 16}
  and class_class = {object' = {class' = class_class; fields = Hashtbl.create 16}; name = "Class"; superclass = Some object_class; traits = []; methods = Hashtbl.create 16}
  and bool_class = {object' = {class' = class_class; fields = Hashtbl.create 16}; name = "Bool"; superclass = Some object_class; traits = []; methods = Hashtbl.create 16}
  and number_class = {object' = {class' = class_class; fields = Hashtbl.create 16}; name = "Number"; superclass = Some object_class; traits = []; methods = Hashtbl.create 16}
  and list_class = {object' = {class' = class_class; fields = Hashtbl.create 16}; name = "List"; superclass = Some object_class; traits = []; methods = Hashtbl.create 16}
  and dict_class = {object' = {class' = class_class; fields = Hashtbl.create 16}; name = "Dict"; superclass = Some object_class; traits = []; methods = Hashtbl.create 16}
  and function_class = {object' = {class' = class_class; fields = Hashtbl.create 16}; name = "Function"; superclass = Some object_class; traits = []; methods = Hashtbl.create 16}
  and string_class = {object' = {class' = class_class; fields = Hashtbl.create 16}; name = "String"; superclass = Some object_class; traits = []; methods = Hashtbl.create 16}
  and primitive_class = {object' = {class' = class_class; fields = Hashtbl.create 16}; name = "Primitive"; superclass = Some object_class; traits = []; methods = Hashtbl.create 16}
  and null_class = {object' = {class' = class_class; fields = Hashtbl.create 16}; name = "Null"; superclass = Some object_class; traits = []; methods = Hashtbl.create 16}
  and trait_class = {object' = {class' = class_class; fields = Hashtbl.create 16}; name = "Trait"; superclass = Some object_class; traits = []; methods = Hashtbl.create 16}

  let make_bool (bool: bool) = EBool (bool, {class' = bool_class; fields = Hashtbl.create 16})
  let make_list (list: t list) = EList (list, {class' = list_class; fields = Hashtbl.create 16})
  let make_dict (dict: (t * t) list) = EDict (Hashtbl.of_seq (List.to_seq dict), {class' = dict_class; fields = Hashtbl.create 16})
  let make_number (number: float) = ENumber (number, {class' = number_class; fields = Hashtbl.create 16})
  let make_function (name: string) (params: string list) (body: Ast.t) (env: Env.t) = {object' = {class' = function_class; fields = Hashtbl.create 16}; name = name; params = params; body = body; env = env}
  let call_function (func: function') (args: t list) (eval: Ast.t -> Env.t -> t) = eval func.body (Env.make (List.combine func.params args) (Some func.env))
  let make_primitive (func: t list -> t) = EPrimitive (func, {class' = primitive_class; fields = Hashtbl.create 16})
  let make_string (string: string) = EString (string, {class' = string_class; fields = Hashtbl.create 16})
  let make_null () = EObject {class' = null_class; fields = Hashtbl.create 16}
  let rec subset (list1: 'a list) (list2: 'a list): bool =
    match list1 with
    | [] -> true
    | x :: xs -> List.mem x list2 && subset xs list2
  let meets_trait_reqs (trait: trait) (methods: (string * t) list): bool =
    subset trait.abs_methods (List.map (fun (n, _) -> n) methods)
  let make_class (name: string) (superclass: class' option) (traits: trait list) (methods: (string * t) list): class' =
    assert (List.for_all (fun x -> meets_trait_reqs x methods) traits);
    {object' = {class' = class_class; fields = Hashtbl.create 16}; name = name; superclass = superclass; traits = traits; methods = Hashtbl.of_seq (List.to_seq methods)}
  let make_trait (name: string) (abs_methods: string list) (methods: (string * t) list): trait =
    {object' = {class' = trait_class; fields = Hashtbl.create 16}; name = name; abs_methods = abs_methods; methods = Hashtbl.of_seq (List.to_seq methods)}
  let object_of = function
  | EObject o -> o
	| EClass c -> c.object'
	| ETrait t -> t.object'
	| EFunction f -> f.object'
	| EBool (_, o) | ENumber (_, o) | EString (_, o) | EList (_, o) | EDict (_, o) | EPrimitive (_, o) ->  o
  let class_of (exp: t) = (object_of exp).class'
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
        | Some m -> m
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
    bind_super (match Hashtbl.find_opt class'.methods name with
    | Some m -> m
    | None -> get_method_from_traits class' name) class'.superclass
  let get_method (exp: t) ?(start_class = class_of exp) (name: string): t =
    (* print_endline ("GET METHOD: " ^ name); *)
    match get_method_from_class start_class name with
    | EFunction f -> EFunction (make_function f.name f.params f.body (Env.make [("self", exp)] (Some f.env)))
    | EPrimitive (p, o) -> EPrimitive ((fun args -> p (exp :: args)), o)
    | _ -> failwith "Not a method"
  let add_method (class': class') (name: string) (method': t) =
    Hashtbl.replace class'.methods name method'
  let dot (exp: t) (name: string): t =
    match Hashtbl.find_opt (object_of exp).fields name with
    | Some f -> f
    | None -> get_method exp name
  let add_field (exp: t) (field: string) (value: t) =
    Hashtbl.replace (object_of exp).fields field value
  let to_class = function
  | EClass c -> c
  | _ -> failwith "Not a class";;
  let to_trait = function
  | ETrait t -> t
  | _ -> failwith "Not a trait";;
  
  add_method string_class "to_string" (make_primitive (fun [self] -> self));
  add_method object_class "to_string" (make_primitive (fun [self] -> make_string ("<" ^ (class_of self).name ^ " instance>")));
  add_method class_class "to_string" (make_primitive (fun [EClass self] -> make_string ("<" ^ self.name ^ " class>")));
  add_method trait_class "to_string" (make_primitive (fun [self] -> make_string ("<" ^ (class_of self).name ^ " trait>")));
  add_method object_class "to_bool" (make_primitive (fun _ -> make_bool true));
  add_method bool_class "to_bool" (make_primitive (fun [self] -> self));
  add_method number_class "to_string" (make_primitive (fun [ENumber (self, _)] -> make_string (Printf.sprintf "%g" self)));
  add_method null_class "to_string" (make_primitive (fun _ -> make_string "null"));
  add_method bool_class "to_string" (make_primitive (fun [EBool (self, _)] -> make_string (string_of_bool self)));
  add_method function_class "to_string" (make_primitive (fun [EFunction self] -> make_string self.name));
  add_method primitive_class "to_string" (make_primitive (fun [EPrimitive (self, _)] -> make_string "<primitive>"));
  add_method object_class "init" (make_primitive (fun _ -> make_null ()));
  add_method class_class "new" (make_primitive (fun (self :: args) -> EObject {class' = to_class self; fields = Hashtbl.create 16}));
  add_method number_class "+" (make_primitive (fun [ENumber (n1, _); ENumber (n2, _)] -> make_number (n1 +. n2)));
  add_method number_class "-" (make_primitive (fun [ENumber (n1, _); ENumber (n2, _)] -> make_number (n1 -. n2)));
  add_method number_class ">" (make_primitive (fun [ENumber (n1, _); ENumber (n2, _)] -> make_bool (n1 > n2)));
  add_method number_class "<" (make_primitive (fun [ENumber (n1, _); ENumber (n2, _)] -> make_bool (n1 < n2)));
  add_method number_class "<=" (make_primitive (fun [ENumber (n1, _); ENumber (n2, _)] -> make_bool (n1 <= n2)));
  add_method number_class "%" (make_primitive (fun [ENumber (n1, _); ENumber (n2, _)] -> make_number (mod_float n1 n2)));
  add_method object_class "==" (make_primitive (fun [self; other] -> make_bool (Hashtbl.hash self = Hashtbl.hash other)));
  add_method list_class "::" (make_primitive (fun [EList (self, _); other] -> make_list (other :: self)));
  add_method bool_class "not" (make_primitive (fun [EBool (self, _)] -> make_bool (not self)));
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