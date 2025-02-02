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
  | EFun (ps, b) -> VFunction (ps, b, env)
  | EVar v -> Env.lookup env v
  | EDot (o, f) -> Value.dot (eval o env) f
  | ECall (f, a) -> call (eval f env) (List.map (fun x -> eval x env) a)
  | EIf (c, t, e) -> eval (if eval c env = VBool true then t else e) env
  | EAssign (n, v) -> Env.bind env n (eval v env)
  | EDef (n, ps, b) -> Env.bind env n (VFunction (ps, b, env))
  | EClass (n, s, t, ms) ->
    Env.bind env n (VClass (Value.make_class
      (match s with Some s' -> Some (eval s' env) | None -> None)
      (match t with Some t' -> Some (eval t' env) | None -> None)
      (List.map (fun (n, ps, b) -> (n, Value.VFunction (ps, b, env))) ms)
    ))
  | ETrait (n, ams, ms) -> Env.bind env n (VTrait (Value.make_trait ams (List.map (fun (n, ps, b) -> (n, Value.VFunction (ps, b, env))) ms)))
and call func args =
  match func with
  | VFunction (ps, b, e) -> eval b (Env.create (List.combine ps args) (Some e))
  | VPrimitive p -> p args
  | VClass _ -> call (Value.dot func "new") []
  | _ -> failwith "Not a function"

let to_string obj =
  match call (Value.dot obj "to_string") [] with
  | VString s -> s
  | _ -> failwith "Not a string";;

Value.add_methods Value.class_class [("new", fun (Value.VClass self :: args) -> let obj = Value.VObject {class' = self; fields = Hashtbl.create 16} in call (Value.dot obj "init") args; obj)];
Value.add_methods Value.list_class [("to_string", fun (Value.VList self :: _) -> Value.VString (String.concat ", " (List.map to_string self)))];