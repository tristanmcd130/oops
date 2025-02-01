let rec eval (exp: Exp.t) env =
  match exp with
  | EBlock [e] -> eval e env
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
  | EIf ((c, t) :: bs) -> eval (if (call (Value.dot (eval c env) "to_bool") []).repr = RBool true then t else EIf bs) env
  | EWhile (c, b) ->
    while (eval c env).repr = RBool true do
      eval b env |> ignore
    done;
    Value.make_null ()
  | EFor (n, l, b) ->
    let iter = call (Value.dot (eval l env) "iterator") [] in
    while (call (Value.dot iter "done") []).repr <> RBool true do
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
    Value.make_class n (match s with Some e -> Some (eval e env |> Value.to_class) | None -> None) ms sfs sms |> Value.of_class |> Env.bind env n;
    Value.make_null ()
and call func args =
  match func.repr with
  | RFunction (_, ps, b, e) -> eval b (Env.create (List.combine ps args) (Some e))
  | RPrimitive p -> p args
  | RClass _ -> call (Value.dot func "new") []
  | _ -> failwith "Not a function"
and eval_class_defs defs env = List.fold_left (
  fun (ms, sfs, sms) (d: Exp.class_def) -> match d with 
  | CAssign (n, v) -> (ms, (n, eval v env) :: sfs, sms)
  | CFun (n, ps, b) -> ((n, Value.make_function n ps b env) :: ms, sfs, sms)
  | CStaticFun (n, ps, b) -> (ms, sfs, (n, Value.make_function n ps b env) :: sms)
) ([], [], []) defs;;

Value.add_methods Value.object_metaclass [
  ("new", fun ({repr = RClass self} :: args) ->
    let obj: Value.t = {class' = self; repr = RObject (Hashtbl.create 16)} in
    call (Value.dot obj "init") args;
    obj
  )
]

let to_string value =
  match (call (Value.dot value "to_string") []).repr with
  | RString s -> s
  | _ -> failwith "Not a string"