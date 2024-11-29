open Exp

let rec eval (ast: Ast.t) (env: Env.t): Exp.t =
  match ast with
  | ABlock [] | ANull | AIf [] -> Exp.make_null ()
  | ABlock [b] -> eval b env
  | ABlock (b :: bs) -> ignore (eval b env); eval (ABlock bs) env
  | ABool b -> Exp.make_bool b
  | ANumber n -> Exp.make_number n
  | AString s -> Exp.make_string s
  | AList l -> Exp.make_list (List.map (fun x -> eval x env) l)
  | ADict d -> Exp.make_dict (List.map (fun (x, y) -> (eval x env, eval y env)) d)
  | ALambda (ps, b) -> EFunction (Exp.make_function "lambda" ps b env)
  | AVar v -> Env.find env v
  | AAssign (n, v) ->
    let v' = eval v env in
    Env.add env n v';
    v'
  | ADotAssign (e, f, v) ->
    let e' = eval e env in
    let v' = eval v env in
    Exp.add_field e' f v';
    v'
  | ADef (n, p, b) -> 
    let f: Exp.t = EFunction (Exp.make_function n p b env) in
    Env.add env n f;
    f
  | ACall (f, a) -> call (eval f env) (List.map (fun x -> eval x env) a)
  | ADot (e, f) -> Exp.dot (eval e env) f
  | ASuper (n, a) ->
    let self = Env.find env "self" in
    let super = Env.find env "super" in
    call (Exp.get_method self ?start_class: (Some (Exp.to_class super)) n) (List.map (fun x -> eval x env) a)
  | AClass (n, s, ts, ms) ->
    let s' = match s with Some x -> Env.find env x | None -> EClass Exp.object_class in
    let ts' = List.map (fun t -> Exp.to_trait (Env.find env t)) ts in
    let ms' = List.map (fun (m, ps, b) -> (m, Exp.EFunction (Exp.make_function (n ^ "." ^ m) ps b env))) ms in
    let c: Exp.t = EClass (Exp.make_class n (Some (Exp.to_class s')) ts' ms') in
    Env.add env n c;
    c
  | ATrait (n, ams, ms) ->
    let ms' = List.map (fun (m, ps, b) -> (m, Exp.EFunction (Exp.make_function (n ^ "." ^ m) ps b env))) ms in
    let t: Exp.t = ETrait (Exp.make_trait n ams ms') in
    Env.add env n t;
    t
  | AIf ((c, b) :: cs) -> if bool_of_exp (eval c env) then eval b env else eval (AIf cs) env
and call (func: Exp.t) (args: Exp.t list): Exp.t =
  match func with
  | EFunction f -> Exp.call_function f args eval
  | EPrimitive (p, _) -> p args
  | EClass _ ->
    let obj = call (Exp.get_method func "new") args in
    ignore (call (Exp.get_method obj "init") args);
    obj
  | _ -> failwith "Not callable"
and string_of_exp (object': Exp.t): string =
  match call (Exp.get_method object' "to_string") [] with
  | EString (s, _) -> s
  | _ -> failwith "Not a string"
and bool_of_exp (object': Exp.t): bool =
  match call (Exp.get_method object' "to_bool") [] with
  | EBool (b, _) -> b
  | _ -> failwith "Not a bool";;

Exp.add_method Exp.object_class "print" (Exp.make_primitive (fun [self] -> print_endline (match call (Exp.get_method self "to_string") [] with EString (s, _) -> s | _ -> failwith "Not a string"); Exp.make_null ()));
Exp.add_method Exp.list_class "to_string" (Exp.make_primitive (fun [EList (self, _)] -> Exp.make_string ("[" ^ (String.concat ", " (List.map string_of_exp self)) ^ "]")));
Exp.add_method Exp.dict_class "to_string" (Exp.make_primitive (fun [EDict (self, _)] -> Exp.make_string ("{" ^ (String.concat ", " (List.map (fun (x, y) -> (string_of_exp x) ^ ": " ^ (string_of_exp y)) (self |> Hashtbl.to_seq |> List.of_seq))) ^ "}")));