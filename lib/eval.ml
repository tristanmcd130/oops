open Lexing

let rec eval exp env =
  match exp with
  | Exp.Block [e] -> eval e env
  | Block [] | Null | Match (_, []) -> Value.Null
  | Block (e :: es) ->
    eval e env |> ignore;
    eval (Block es) env
  | Bool b -> Bool b
  | Number n -> Number n
  | String s -> String s
  | List l -> List (List.map (fun x -> eval x env) l)
  | Dict d -> Dict (List.map (fun (k, v) -> (eval k env, eval v env)) d |> List.to_seq |> Hashtbl.of_seq)
  | Fun (ps, b) -> Function ("", ps, b, env)
  | Var v -> Env.lookup env v
  | Dot (o, f) -> Value.dot (eval o env) f
  | Call (f, a) -> call (eval f env) (List.map (fun x -> eval x env) a)
  | If ((c, b) :: cs) -> eval (if eval c env = Bool true then b else If cs) env
  | Match (e, cs) ->
    let v = eval e env in
    (match List.find_map (fun (p, b) -> Option.bind (Value.match' p v) (fun e -> Some (eval b (Env.create e (Some env))))) cs with
    | Some v' -> v'
    | None -> Null)
  | Let ([], b) -> eval b env
  | Let ((p, v) :: ds, b) -> eval (Let (ds, b)) (Env.create (Value.match' p (eval v env) |> Option.get) (Some env))
  | Try (b, cs) ->
    (try
      eval b env
    with
    | Value.Runtime_error e ->
      (match List.find_map (fun (p, b) -> Option.bind (Value.match' p e) (fun e' -> Some (eval b (Env.create e' (Some env))))) cs with
      | Some v' -> v'
      | None -> Null))
  | Throw e -> raise (Value.Runtime_error (eval e env))
  | Assign (p, v) ->
    Env.bind_list env (Value.match' p (eval v env) |> Option.value ~default: []);
    Null
  | DotAssign (o, f, v) ->
    Value.dot_assign (eval o env) f (eval v env);
    Null
  | Def (n, ps, b) ->
    Env.bind env n (Function (n, ps, b, env));
    Null
  | Struct (n, fs) ->
    Env.bind env n (Type (Value.make_type n fs));
    Null
  | Trait (n, ams, ms) ->
    Env.bind env n (Trait (Value.make_trait n ams (List.map (fun (n', ps, b) -> (n', Value.Function (n ^ "." ^ n', ps, b, env))) ms)));
    Null
  | Impl (tr, ty, ms) ->
    let tr' = Option.bind (Option.bind tr (fun x -> Some (eval x env))) (fun (Trait x) -> Some x) in
    let ty' = eval ty env in
    Value.impl tr' ty' (List.map (fun (n, ps, b) -> (n, Value.Function (Value.type_name ty' ^ "." ^ n, ps, b, env))) ms);
    Null
  | Module (n, es, b) ->
    let e = Env.create [] (Some env) in
    eval b e |> ignore;
    Env.bind env n (Struct (Value.module_type, (List.map (fun x -> (x, Env.lookup e x)) es @ [("__name", String n)]) |> List.to_seq |> Hashtbl.of_seq));
    Null
  | Import f ->
    run_file f env;
    Null
and call func args =
  match func with
  | Function (_, ps, b, e) -> eval b (Env.create (List.combine ps args) (Some e))
  | Primitive p -> p args
  | Type t -> Value.make_struct t args
  | _ -> failwith (to_string func ^ " is not a function")
and run_file filename env =
  try
    eval (In_channel.open_text filename |> from_channel |> Parser.prog Lexer.read) env |> ignore
  with
  | Value.Runtime_error e -> print_endline ("Uncaught error: " ^ to_string e)
  | e -> print_endline ("Uncaught primitive error: " ^ Printexc.to_string e)
and to_string obj =
  match call (Value.dot obj "to_string") [] with
  | String s -> s
  | _ -> failwith "Not a string";;

let rec format string values =
  if String.length string = 0 then
    ""
  else
    match string.[0] |> String.make 1 with
    | "\\" -> (string.[1] |> String.make 1) ^ format (String.sub string 2 (String.length string - 2)) values
    | "{" ->
      let num = Scanf.sscanf (String.sub string 1 (String.length string - 1)) "%u" (fun x -> x) in
      let num_len = num |> string_of_int |> String.length in
      assert (string.[num_len + 1] |> String.make 1 = "}");
      (List.nth values num |> to_string) ^ format (String.sub string (num_len + 2) (String.length string - num_len - 2)) values
    | x -> x ^ format (String.sub string 1 (String.length string - 1)) values;;

Value.impl (Some Value.printable_trait) (Trait Value.base_trait) [
  ("to_string", Primitive (fun [Struct (t, fs)] -> String ((Type t |> Value.type_name) ^ "(" ^ (List.map (Hashtbl.find fs) (Value.fields t) |> List.map to_string |> String.concat ", ") ^ ")")));
  ("==", Primitive (fun [self; other] -> Bool (self = other)));
  ("!=", Primitive (fun [self; other] -> Bool (self <> other)));
];
Value.impl None (Type Value.string_type) [
  ("format", Primitive (fun [String self; List args] -> String (format self args)));
];
Value.impl (Some Value.printable_trait) (Type Value.list_type) [
  ("to_string", Primitive (fun [List self] -> String ("[" ^ String.concat ", " (List.map to_string self) ^ "]")));
];
Value.impl (Some Value.printable_trait) (Type Value.dict_type) [
  ("to_string", Primitive (fun [Dict self] -> String ("{" ^ String.concat ", " (Hashtbl.to_seq self |> List.of_seq |> List.map (fun (k, v) -> to_string k ^ ": " ^ to_string v)) ^ "}")));
];