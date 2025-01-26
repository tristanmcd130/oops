let rec eval (exp: Exp.t) env =
  match exp with
  | EBlock [] | ENull -> Value.make_null ()
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
  | ECall (f, a) -> Value.call (eval f env) (List.map (fun x -> eval x env) a) eval;;