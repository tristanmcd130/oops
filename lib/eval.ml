open Lexing

let rec eval (exp: Exp.t) env: Value.t =
  match exp with
  | EBlock [e] -> eval e env
  | EBlock [] | ENull | ECond [] | EMatch (_, []) -> VNull
  | EBlock (e :: es) ->
    eval e env |> ignore;
    eval (EBlock es) env
  | EBool b -> VBool b
  | ENumber n -> VNumber n
  | EString s -> VString s
  | EList l -> VList (List.map (fun x -> eval x env) l)
  | EDict d -> VDict (List.map (fun (k, v) -> (eval k env, eval v env)) d |> List.to_seq |> Hashtbl.of_seq)
  | EFun (ps, b) -> VFunction ("", ps, b, env)
  | EVar v -> Env.lookup env v
  | EDot (o, f) -> Value.dot (eval o env) f
  | ECall (f, a) -> call (eval f env) (List.map (fun x -> eval x env) a)
  | EIf (c, t, e) -> eval (if eval c env = VBool true then t else e) env
  | ECond ((c, b) :: cs) -> eval (if eval c env = VBool true then b else ECond cs) env
  | EMatch (e, cs) ->
    let v = eval e env in
    (match List.find_map (fun (p, b) -> Option.bind (match' p v) (fun e -> Some (eval b (Env.create e (Some env))))) cs with
    | Some v' -> v'
    | None -> VNull)
  | ELet ([], b) -> eval b env
  | ELet ((p, v) :: ds, b) -> eval (ELet (ds, b)) (Env.create (match' p (eval v env) |> Option.get) (Some env))
  | ETry (b, cs) ->
    (try
      eval b env
    with
    | Value.Runtime_error e -> (match List.find_map (fun (p, b) -> Option.bind (match' p e) (fun e' -> Some (eval b (Env.create e' (Some env))))) cs with
      | Some v' -> v'
      | None -> VNull))
  | EThrow e -> raise (Value.Runtime_error (eval e env))
  | EAssign (p, v) ->
    Env.bind_list env (match' p (eval v env) |> Option.get);
    VNull
  | EDotAssign (o, f, v) ->
    (match eval o env with
    | VStruct s ->
      (if Hashtbl.mem s.fields f then
        Hashtbl.replace s.fields f (eval v env)
      else
        Value.throw Value.field_undefined_error_type (Value.type_name (VType s.type') ^ " does not have field " ^ f));
      VNull
    | _ -> Value.throw Value.field_undefined_error_type "Primitive values have no fields")
  | EDef (n, ps, b) ->
    Env.bind env n (VFunction (n, ps, b, env));
    VNull
  | EStruct (n, fs, ms) ->
    Env.bind env n (Value.make_type n fs (List.map (fun (n', ps, b) -> (n', Value.VFunction (n ^ "." ^ n', ps, b, env))) ms));
    VNull
  | ETrait (n, ams, ms) ->
    Env.bind env n (Value.make_trait n ams (List.map (fun (n', ps, b) -> (n', Value.VFunction (n ^ "." ^ n', ps, b, env))) ms));
    VNull
  | EImpl (tr, ty, ms) ->
    let tr' = Option.bind (Option.bind tr (fun x -> Some (eval x env))) (fun (VTrait x) -> Some x) in
    let ty' = eval ty env in
    Value.impl tr' ty' (List.map (fun (n, ps, b) -> (n, Value.VFunction (Value.type_name ty' ^ "." ^ n, ps, b, env))) ms);
    VNull
  | EModule (n, es, b) ->
    let e = Env.create [] (Some env) in
    eval b e;
    Env.bind env n (VStruct {type' = Value.module_type; fields = (List.map (fun x -> (x, Env.lookup e x)) es @ [("__name", VString n)]) |> List.to_seq |> Hashtbl.of_seq});
    VNull
  | EImport f ->
    run_file f env;
    VNull
and call func args =
  match func with
  | VFunction (_, ps, b, e) -> eval b (Env.create (List.combine ps args) (Some e))
  | VPrimitive p -> p args
  | VType t -> Value.make_struct t args
  | _ -> failwith "Not a function"
and run_file filename env =
  try
    eval (In_channel.open_text filename |> from_channel |> Parser.prog Lexer.read) env |> ignore
  with
  | Value.Runtime_error e -> print_endline ("Uncaught error: " ^ to_string e)
  | e -> print_endline ("Uncaught primitive error: " ^ Printexc.to_string e)
and match' pattern value =
  match (pattern, value) with
  | (Exp.ENull, Value.VNull) -> Some []
  | (EBool b, VBool b') when b = b' -> Some []
  | (ENumber n, VNumber n') when n = n' -> Some []
  | (EString s, VString s') when s = s' -> Some []
  | (EList [], VList []) -> Some []
  | (EList (p :: ps), VList (v :: vs)) ->
    (match match' p v with
    | Some bs ->
      (match match' (EList ps) (VList vs) with
      | Some bs' -> Some (bs' @ bs)
      | None -> None)
    | None -> None)
  (* TODO: add dict *)
  | (EVar "_", _) -> Some []
  | (EVar n, v) -> Some [(n, v)]
  | (ECall (EDot (ps, "::"), [p]), VList (v :: vs)) ->
    (match match' p v with
    | Some bs ->
      (match match' ps (VList vs) with
      | Some bs' -> Some (bs' @ bs)
      | None -> None)
    | None -> None)
  | (ECall (EVar s, ps), VStruct {type' = t; fields = fs}) when s = Value.type_name (VType t) -> match' (EList ps) (VList (fs |> Hashtbl.to_seq_values |> List.of_seq))
  | (ECall (EDot (p1, "or"), [p2]), v) ->
    (match match' p1 v with
    | Some bs -> Some bs
    | None -> match' p2 v)
  | (ECall (EDot (p1, "and"), [p2]), v) ->
    (match match' p1 v with
    | Some bs ->
      (match match' p2 v with
      | Some bs' -> Some (bs' @ bs)
      | None -> None)
    | None -> None)
  | _ -> None
and to_string obj =
  match call (Value.dot obj "to_string") [] with
  | VString s -> s
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

Value.impl None (VTrait Value.base_trait) [
  ("to_string", VPrimitive (fun [VStruct self] -> VString ((VType self.type' |> Value.type_name) ^ "(" ^ (self.fields |> Hashtbl.to_seq_values |> List.of_seq |> List.map to_string |> String.concat ", ") ^ ")")));
  ("==", VPrimitive (fun [self; other] -> VBool (self = other)));
  ("!=", VPrimitive (fun [self; other] -> VBool (self <> other)));
];
Value.impl None (VType Value.string_type) [
  ("format", VPrimitive (fun [VString self; VList args] -> VString (format self args)));
];
Value.impl (Some Value.printable_trait) (VType Value.list_type) [
  ("to_string", VPrimitive (fun [VList self] -> VString ("[" ^ String.concat ", " (List.map to_string self) ^ "]")));
];
Value.impl (Some Value.printable_trait) (VType Value.dict_type) [
  ("to_string", VPrimitive (fun [VDict self] -> VString ("{" ^ String.concat ", " (Hashtbl.to_seq self |> List.of_seq |> List.map (fun (k, v) -> to_string k ^ ": " ^ to_string v)) ^ "}")));
];