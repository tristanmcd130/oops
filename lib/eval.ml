let rec eval (exp: Exp.t) env: Value.t =
  match exp with
  | EBlock [e] -> eval e env
  | EBlock [] | ENull -> VNull
  | EBlock (e :: es) ->
    eval e env |> ignore;
    eval (EBlock es) env
  | EBool b -> VBool b
  | ENumber n -> VNumber n
  | EString s -> VString s
  | EList l -> VList (List.map (fun x -> eval x env) l)
  | EDict d -> VDict (List.map (fun (k, v) -> (eval k env, eval v env)) d |> List.to_seq |> Hashtbl.of_seq)
  | EFun (ps, b) -> VFunction (ps, b, env)
  | EVar v -> Env.lookup env v
  | EDot (o, f) -> Value.dot (eval o env) f
  | ECall (f, a) -> call (eval f env) (List.map (fun x -> eval x env) a)
  | EIf (c, t, e) -> eval (if eval c env = VBool true then t else e) env
  | ELet ([], b) -> eval b env
  | ELet ((n, v) :: ds, b) -> eval (ELet (ds, b)) (Env.create [(n, eval v env)] (Some env))
  | EAssign (n, v) ->
    Env.bind env n (eval v env);
    VNull
  | EDotAssign (o, f, v) ->
    (match eval o env with
    | VStruct s ->
      Hashtbl.replace s.fields f (eval v env);
      VNull
    | _ -> failwith "Cannot assign to fields of primitive")
  | EDef (n, ps, b) ->
    Env.bind env n (VFunction (ps, b, env));
    VNull
  | EStruct (n, fs) ->
    Env.bind env n (Value.make_type fs);
    VNull
  | ETrait (n, ams, ms) ->
    Env.bind env n (Value.make_trait ams (List.map (fun (n, ps, b) -> (n, Value.VFunction (ps, b, env))) ms));
    VNull
  | EImpl (tr, ty, ms) ->
    let tr' = Option.bind tr (fun x -> Some (eval x env)) in
    Value.impl tr' (eval ty env) (List.map (fun (n, ps, b) -> (n, Value.VFunction (ps, b, env))) ms);
    VNull
and call func args =
  match func with
  | VFunction (ps, b, e) -> eval b (Env.create (List.combine ps args) (Some e))
  | VPrimitive p -> p args
  | VType t -> Value.make_struct t args
  | _ -> failwith "Not a function"  

let to_string obj =
  match call (Value.dot obj "to_string") [] with
  | VString s -> s
  | _ -> failwith "Not a string";;

Value.impl None (VType Value.list_type) [
  ("to_string", VPrimitive (fun [VList self] -> VString ("[" ^ String.concat ", " (List.map to_string self) ^ "]")));
];
Value.impl None (VType Value.dict_type) [
  ("to_string", VPrimitive (fun [VDict self] -> VString ("{" ^ String.concat ", " (Hashtbl.to_seq self |> List.of_seq |> List.map (fun (k, v) -> to_string k ^ ": " ^ to_string v)) ^ "}")));
];