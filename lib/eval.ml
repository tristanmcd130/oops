let rec eval (exp: Exp.t) env =
  match exp with
  | EBlock [] | ENull | EIf [] -> Value.make_null ()
  | EBlock (e :: es) ->
    eval e env |> ignore;
    eval (EBlock es) env
  | EBool b -> Value.make_bool b
  | ENumber n -> Value.make_number n
  | EString s -> Value.make_string s
  | EList l -> Value.make_list (List.map (fun x -> eval x env) l)
  | ELambda (ps, b) -> Value.make_function "" ps b env
  | EVar v -> Env.lookup env v
  | EDot (o, f) -> Value.dot (eval o env) f
  | ECall (f, a) -> call (eval f env) (List.map (fun x -> eval x env) a)
  | EIf ((c, t) :: bs) -> eval (if Value.to_bool (call (Value.dot (eval c env) "to_bool") []) then t else EIf bs) env
  | EWhile (c, b) ->
    while Value.to_bool (eval c env) do
      eval b env |> ignore
    done;
    Value.make_null ()
  | EFor (n, l, b) ->
    let iter = eval l env in
    while not (Value.to_bool (call (Value.dot iter "done") [])) do
      eval b (Env.create [(n, call (Value.dot iter "next") [])] (Some env)) |> ignore
    done;
    Value.make_null ()
  | EAssign (n, v) ->
    Env.bind env n (eval v env);
    Value.make_null ()
  | EFun (n, ps, b) ->
    Env.bind env n (Value.make_function n ps b env);
    Value.make_null ()
  | EClass (n, s, ds) ->
    let (ms, sfs, sms) = eval_class_defs ds env in
    Value.make_class n (match s with Some n -> Some (Env.lookup env n |> Value.to_class) | None -> None) ms sfs sms |> Value.of_class |> Env.bind env n;
    Value.make_null ()
and call func args =
  match func.repr with
  | RFunction (_, ps, b, e) -> eval b (Env.create (List.combine ps args) (Some e))
  | RPrimitive p -> p args
  | RClass _ -> call (Value.dot func "new") []
  | _ -> failwith "Not a function"
and eval_class_defs defs env = List.fold_left (fun (ms, sfs, sms) (d: Exp.class_def) -> match d with 
| CAssign (n, v) -> (ms, (n, eval v env) :: sfs, sms)
| CFun (n, ps, b) -> ((n, Value.make_function n ps b env) :: ms, sfs, sms)
| CStaticFun (n, ps, b) -> (ms, sfs, (n, Value.make_function n ps b env) :: sms)) ([], [], []) defs;;

Hashtbl.replace Value.object_metaclass.methods "new" (Value.make_primitive (fun ({repr = RClass self} :: args) ->
  let obj: Value.t = {class' = self; repr = RObject (Hashtbl.create 16)} in
  call (Value.dot obj "init") args;
  obj))