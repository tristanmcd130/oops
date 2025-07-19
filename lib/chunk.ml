open Types

type t = chunk

let empty () = {code = [||]; constants = [||]; names = [||]; handlers = []}
let rec to_string chunk =
  "Code:\n" ^ (Array.mapi (fun i x -> string_of_int i ^ ": " ^ Opcode.to_string x) chunk.code |> Array.to_list |> String.concat "\n")
  ^ "\n\nConstants:\n" ^ (Array.mapi (fun i x -> string_of_int i ^ ": " ^ Value.to_string x) chunk.constants |> Array.to_list |> String.concat "\n")
  ^ "\n\nNames:\n" ^ (Array.mapi (fun i x -> string_of_int i ^ ": " ^ x) chunk.names |> Array.to_list |> String.concat "\n")
  ^ "\n\nException handlers:\n" ^ (List.map (fun (f, t) -> string_of_int f ^ "-" ^ string_of_int t ^ ": " ^ string_of_int (t + 1)) chunk.handlers |> String.concat "\n")
let rec find_first value = function
| [||] -> None
| a ->
  try
    if a.(0) = value || a.(0) == value then
      Some 0
    else
      match Array.length a - 1 |> Array.sub a 1 |> find_first value with
      | None -> None
      | Some i -> Some (i + 1)
  with
  | Invalid_argument _ -> None
let add_opcode chunk opcode =
  chunk.code <- Array.append chunk.code [|opcode|];
  Array.length chunk.code - 1
let add_constant chunk constant =
  match find_first constant chunk.constants with
  | None ->
    chunk.constants <- Array.append chunk.constants [|constant|];
    Array.length chunk.constants - 1
  | Some i -> i
let add_name chunk name =
  match find_first name chunk.names with
  | None ->
    chunk.names <- Array.append chunk.names [|name|];
    Array.length chunk.names - 1
  | Some i -> i
let length chunk = Array.length chunk.code
let rec compile chunk scope module' = function
| Ast.Block [] -> compile chunk scope module' (List [])
| Block [x] -> compile chunk scope module' x
| Block (x :: xs) ->
  compile chunk scope module' x;
  compile chunk scope module' (Block xs)
| Bool b -> add_opcode chunk (GetConstant (add_constant chunk (Bool b))) |> ignore
| Number n -> add_opcode chunk (GetConstant (add_constant chunk (Number n))) |> ignore
| String s -> add_opcode chunk (GetConstant (add_constant chunk (String s))) |> ignore
| List l ->
  List.iter (compile chunk scope module') l;
  add_opcode chunk (MakeList (List.length l)) |> ignore
| Map m ->
  List.iter (fun (k, v) -> compile chunk scope module' k; compile chunk scope module' v) m;
  add_opcode chunk (MakeMap (List.length m)) |> ignore
| Var n ->
  (match Scope.get_level scope n with
  | Global -> add_opcode chunk (GetGlobal (add_name chunk n))
  | Upvalue -> add_opcode chunk (DerefUpvalue (Scope.add_upvalue scope n))
  | Local -> add_opcode chunk (GetLocal (Scope.find_local scope n))) |> ignore
| Assign (n, v) ->
  compile chunk scope module' v;
  (match scope.parent with
  | None -> add_opcode chunk (SetGlobal (add_name chunk n))
  | Some _ -> add_opcode chunk (SetLocal (Scope.add_local scope n))) |> ignore
| Fun (n, ps, b) ->
  let c = empty () in
  let s = Scope.make (Some scope) ps in
  Scope.resolve_locals s b;
  tail_compile c s module' b;
  s.upvalues |> Hashtbl.iter (fun n (i, l) ->
    match l with
    | Scope.Upvalue -> add_opcode chunk (GetUpvalue i) |> ignore
    | Local -> add_opcode chunk (MakeCell i) |> ignore
    | _ -> ());
  add_opcode chunk (GetConstant (Closure {name = n; module'; chunk = c; num_args = List.length ps; num_locals = Hashtbl.length s.locals - List.length ps; upvalues = [||]} |> add_constant chunk)) |> ignore;
  if s.upvalues |> Hashtbl.length > 0 then
    add_opcode chunk (Enclose (s.upvalues |> Hashtbl.length)) |> ignore
| Call (f, a) ->
  List.iter (compile chunk scope module') a;
  compile chunk scope module' f;
  add_opcode chunk (Call (List.length a)) |> ignore
| If bs ->
  let rec compile_if branches =
    (match branches with
    | [] -> failwith "Empty if"
    | (Ast.Bool true, t) :: _ -> compile chunk scope module' t
    | (c, t) :: bs' ->
      compile chunk scope module' c;
      let j = add_opcode chunk (JumpIfFalse 999) in
      compile chunk scope module' t;
      let j2 = add_opcode chunk (Jump 999) in
      let e = compile_if bs' in
      chunk.code.(j) <- JumpIfFalse (j2 + 1);
      chunk.code.(j2) <- Jump e);
    length chunk in
  compile_if bs |> ignore
| Struct (n, fs) ->
  add_opcode chunk (GetConstant (add_constant chunk (Type {name = n; fields = fs |> List.mapi (fun i n -> (fst n, i)) |> List.to_seq |> Hashtbl.of_seq; default_values = Array.make (List.length fs) None; methods = Hashtbl.create 16; traits = [Value.base_trait]}))) |> ignore;
  List.iter (fun (n, v) ->
    match v with
    | None -> ()
    | Some v' -> compile chunk scope module' v'; add_opcode chunk (SetDefault (add_name chunk n)) |> ignore) fs
| Dot (o, f) ->
  compile chunk scope module' o;
  add_opcode chunk (GetField (add_name chunk f)) |> ignore
| DotAssign (o, f, v) ->
  compile chunk scope module' o;
  compile chunk scope module' v;
  add_opcode chunk (SetField (add_name chunk f)) |> ignore
| Impl (t, ty, ms) ->
  compile chunk scope module' ty;
  List.iter (fun (n, ps, b) -> compile chunk scope module' (Fun (n, "self" :: ps, b)); add_opcode chunk (AddMethod (add_name chunk n)) |> ignore) ms;
  (match t with
  | None -> add_opcode chunk BaseTrait |> ignore
  | Some t' -> compile chunk scope module' t');
  add_opcode chunk Impl |> ignore
| Trait (n, rs, ps) ->
  add_opcode chunk (GetConstant (add_constant chunk (Trait {name = n; requires = rs; provides = Hashtbl.create 16; traits = []}))) |> ignore;
  List.iter (fun (n', ps, b) -> compile chunk scope module' (Fun (n', "self" :: ps, b)); add_opcode chunk (AddMethod (add_name chunk n')) |> ignore) ps
| Import (f, o) ->
  add_opcode chunk (Import (add_name chunk f)) |> ignore;
  (match o with
  | Left a (* as *) ->
    let n = match a with
    | None ->
      let n' = String.split_on_char '/' f |> List.rev |> List.hd in
      String.sub n' 0 (String.index n' '.')
    | Some n' -> n' in
    add_opcode chunk (SetGlobal (add_name chunk n)) |> ignore
  | Right ns (* for *) -> List.iter (fun (n1, n2) -> add_opcode chunk (ImportFor (add_name chunk n1)) |> ignore; add_opcode chunk (SetGlobal (add_name chunk n2)) |> ignore) ns)
| Export ns -> Module.export module' ns
| Throw e ->
  compile chunk scope module' e;
  add_opcode chunk Throw |> ignore
| Try (t, n, c) ->
  let s = length chunk in
  compile chunk scope module' t;
  let e = add_opcode chunk (Jump 999) in
  compile chunk scope module' (Fun ("", [n], c));
  add_opcode chunk (Call 1) |> ignore;
  chunk.code.(e) <- Jump (length chunk);
  chunk.handlers <- (s, e) :: chunk.handlers
| Match (e, cs) ->
  compile chunk scope module' e;
  let c = empty () in
  let s = Scope.make (Some scope) ["tmp!"] in
  let rec compile_cases = function
  | [] ->
    add_opcode c (GetConstant (add_constant c (String "Match unsuccessful"))) |> ignore;
    add_opcode c Throw + 1
  | (p, b) :: cs ->
    let js = ref [] in (* jumps to next case in current case *)
    let rec compile_pattern = function
    | (Ast.Bool _ | Number _ | String _ | List []) as l ->
      compile c s module' (Dot (l, "=="));
      add_opcode c (Call 1) |> ignore;
      js := add_opcode c (JumpIfFalse 999) :: !js
    | List (h :: t) -> compile_pattern (Call (Dot (List t, "::"), [h]))
    | Var "_" -> ()
    | Var v -> add_opcode c (SetLocal (Scope.add_local s v)) |> ignore
    | Call (Dot (t, "::"), [h]) ->
      add_opcode c Dup |> ignore;
      add_opcode c Dup |> ignore;
      add_opcode c GetType |> ignore;
      add_opcode c (GetConstant (add_constant c (Type Value.list_type))) |> ignore;
      add_opcode c (GetField (add_name c "==")) |> ignore;
      add_opcode c (Call 1) |> ignore;
      js := add_opcode c (JumpIfFalse 999) :: !js;
      add_opcode c (GetField (add_name c "head")) |> ignore;
      add_opcode c (Call 0) |> ignore;
      compile_pattern h;
      add_opcode c (GetField (add_name c "tail")) |> ignore;
      add_opcode c (Call 0) |> ignore;
      compile_pattern t
    | Call (t, [Map fs]) ->
      List.iter (fun (Ast.String k, v) -> add_opcode c Dup |> ignore) fs;
      add_opcode c GetType |> ignore;
      compile c s module' (Dot (t, "=="));
      add_opcode c (Call 1) |> ignore;
      js := add_opcode c (JumpIfFalse 999) :: !js;
      List.iter (fun (Ast.String k, v) ->
        add_opcode c (GetField (add_name c k));
        compile_pattern v) fs
    | _ -> failwith "Cannot compile this pattern" in
    add_opcode c (GetLocal 0) |> ignore;
    compile_pattern p;
    compile c s module' b;
    let je = add_opcode c (Jump 999) in
    let jd = compile_cases cs in
    c.code.(je) <- Jump jd;
    List.iter (fun i -> c.code.(i) <- JumpIfFalse (je + 1)) !js;
    jd in
  compile_cases cs |> ignore;
  s.upvalues |> Hashtbl.iter (fun n (i, l) ->
    match l with
    | Scope.Upvalue -> add_opcode chunk (GetUpvalue i) |> ignore
    | Local -> add_opcode chunk (MakeCell i) |> ignore
    | _ -> ());
  add_opcode chunk (GetConstant (Closure {name = ""; module'; chunk = c; num_args = 1; num_locals = Hashtbl.length s.locals - 1; upvalues = [||]} |> add_constant chunk)) |> ignore;
  if s.upvalues |> Hashtbl.length > 0 then
    add_opcode chunk (Enclose (s.upvalues |> Hashtbl.length)) |> ignore;
  add_opcode chunk (Call 1) |> ignore
and tail_compile chunk scope module' = function
| Ast.Block [] -> ()
| Block [x] -> tail_compile chunk scope module' x
| Block (x :: xs) ->
  compile chunk scope module' x;
  tail_compile chunk scope module' (Block xs)
| Call (f, a) ->
  List.iter (compile chunk scope module') a;
  compile chunk scope module' f;
  add_opcode chunk (TailCall (List.length a)) |> ignore
| If bs ->
  let rec compile_if branches =
    (match branches with
    | [] -> failwith "Empty if"
    | (Ast.Bool true, t) :: _ -> tail_compile chunk scope module' t
    | (c, t) :: bs' ->
      compile chunk scope module' c;
      let j = add_opcode chunk (JumpIfFalse 999) in
      tail_compile chunk scope module' t;
      let j2 = add_opcode chunk (Jump 999) in
      let e = compile_if bs' in
      chunk.code.(j) <- JumpIfFalse (j2 + 1);
      chunk.code.(j2) <- Jump e);
    length chunk in
  compile_if bs |> ignore
| x -> compile chunk scope module' x
let to_closure chunk module' = {name = ""; module'; chunk; num_args = 0; num_locals = 0; upvalues = [||]}