open Types

type t = chunk

let empty () = {code = [||]; constants = [||]; names = [||]; handlers = []}
let rec find_first value = function
| [||] -> None
| a ->
  try
    if a.(0) = value then
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
| Ast.Block [] -> ()
| Block (x :: xs) ->
  compile chunk scope module' x;
  compile chunk scope module' (Block xs)
| Null -> add_opcode chunk (GetConstant (add_constant chunk Null)) |> ignore
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
| Struct (n, fs) -> add_opcode chunk (GetConstant (add_constant chunk (Type {name = n; fields = fs |> List.mapi (fun i n -> (n, i)) |> List.to_seq |> Hashtbl.of_seq; methods = Hashtbl.create 16; traits = [Value.base_trait]}))) |> ignore
| Dot (e, f) ->
  compile chunk scope module' e;
  add_opcode chunk (Dot (add_name chunk f)) |> ignore
| Impl (t, ty, ms) ->
  compile chunk scope module' ty;
  List.iter (fun (n, ps, b) -> compile chunk scope module' (Fun (n, "self" :: ps, b)); add_opcode chunk (AddMethod (add_name chunk n)) |> ignore) ms;
  (match t with
  | None -> add_opcode chunk BaseTrait |> ignore
  | Some t' -> compile chunk scope module' t');
  add_opcode chunk Impl |> ignore
| Trait (n, rs, ps) ->
  add_opcode chunk (GetConstant (add_constant chunk (Trait {name = n; requires = rs; provides = Hashtbl.create 16}))) |> ignore;
  List.iter (fun (n', ps, b) -> compile chunk scope module' (Fun (n', "self" :: ps, b)); add_opcode chunk (AddMethod (add_name chunk n')) |> ignore) ps
| Import (p, ns) ->
  let n = String.split_on_char '/' p |> List.rev |> List.hd in
  let n = String.sub n 0 (String.index n '.') in
  add_opcode chunk (Import (add_name chunk p)) |> ignore;
  (match (scope.parent, ns) with
  | (None, None) -> add_opcode chunk (SetGlobal (add_name chunk n)) |> ignore
  | (None, Some ns') -> List.iter (fun (n1, n2) -> add_opcode chunk (DupDot (add_name chunk n1)) |> ignore; add_opcode chunk (SetGlobal (add_name chunk n2)) |> ignore) ns'
  | (Some _, None) -> add_opcode chunk (SetLocal (Scope.add_local scope n)) |> ignore
  | (Some _, Some ns') -> List.iter (fun (n1, n2) -> add_opcode chunk (DupDot (add_name chunk n1)) |> ignore; add_opcode chunk (SetLocal (Scope.add_local scope n2)) |> ignore) ns')
| Export ns -> Module.export module' ns
| Throw e ->
  compile chunk scope module' e;
  add_opcode chunk Throw |> ignore
| Try (t, n, c) ->
  let s = length chunk in
  compile chunk scope module' Null;
  compile chunk scope module' t;
  let e = add_opcode chunk (Jump 999) in
  compile chunk scope module' (Fun ("", [n], c));
  add_opcode chunk (Call 1) |> ignore;
  chunk.code.(e) <- Jump (length chunk);
  chunk.handlers <- (s, e) :: chunk.handlers
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
let rec to_string chunk =
  "Code:\n" ^ (Array.mapi (fun i x -> string_of_int i ^ ": " ^ Opcode.to_string x) chunk.code |> Array.to_list |> String.concat "\n")
  ^ "\n\nConstants:\n" ^ (Array.mapi (fun i x -> string_of_int i ^ ": " ^ Value.to_string x) chunk.constants |> Array.to_list |> String.concat "\n")
  ^ "\n\nNames:\n" ^ (Array.mapi (fun i x -> string_of_int i ^ ": " ^ x) chunk.names |> Array.to_list |> String.concat "\n")
  ^ "\n\nException handlers:\n" ^ (List.map (fun (f, t) -> string_of_int f ^ "-" ^ string_of_int t ^ ": " ^ string_of_int (t + 1)) chunk.handlers |> String.concat "\n")