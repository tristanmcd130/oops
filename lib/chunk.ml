type t = {
  mutable code: Opcode.t array;
  mutable constants: t Value.t array;
  mutable names: string array;
}

let make code constants names = {code; constants; names}
let empty () = make [||] [||] [||]
let rec find_first value = function
| [||] -> None
| a ->
  if a.(0) = value then
    Some 0
  else
    match Array.length a - 1 |> Array.sub a 1 |> find_first value with
    | None -> None
    | Some i -> Some (i + 1)
let get_opcode chunk = Array.get chunk.code
let get_constant chunk = Array.get chunk.constants
let get_name chunk = Array.get chunk.names
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
let rec compile chunk scope = function
| Ast.Block [] -> ()
| Block (x :: xs) ->
  compile chunk scope x;
  compile chunk scope (Block xs)
| Null -> add_opcode chunk (GetConstant (add_constant chunk Null)) |> ignore
| Bool b -> add_opcode chunk (GetConstant (add_constant chunk (Bool b))) |> ignore
| Number n -> add_opcode chunk (GetConstant (add_constant chunk (Number n))) |> ignore
| String s -> add_opcode chunk (GetConstant (add_constant chunk (String s))) |> ignore
| List l ->
  List.iter (compile chunk scope) l;
  add_opcode chunk (MakeList (List.length l)) |> ignore
| Map m ->
  List.iter (fun (k, v) -> compile chunk scope k ; compile chunk scope v) m;
  add_opcode chunk (MakeMap (List.length m)) |> ignore
| Var n ->
  (match Scope.get_level scope n with
  | Global -> add_opcode chunk (GetGlobal (add_name chunk n))
  | Upvalue -> add_opcode chunk (DerefUpvalue (Scope.add_upvalue scope n))
  | Local -> add_opcode chunk (GetLocal (Scope.add_local scope n))) |> ignore
| Assign (n, v) ->
  compile chunk scope v;
  (match Scope.parent scope with
  | None -> add_opcode chunk (SetGlobal (add_name chunk n))
  | Some _ -> add_opcode chunk (SetLocal (Scope.add_local scope n))) |> ignore
| Unary (o, e) ->
  compile chunk scope e;
  add_opcode chunk (match o with
  | Negate -> Negate
  | Not -> Not) |> ignore
| Binary (e1, o, e2) ->
  compile chunk scope e1;
  compile chunk scope e2;
  add_opcode chunk (match o with
  | Add -> Add
  | Subtract -> Subtract
  | Multiply -> Multiply
  | Divide -> Divide
  | Modulo -> Modulo
  | LT -> LT
  | LE -> LE
  | EQ -> EQ
  | NE -> NE
  | GT -> GT
  | GE -> GE
  | And -> And
  | Or -> Or
  | Cons -> Cons) |> ignore
| Fun (n, ps, b) ->
  let c = empty () in
  let s = Scope.make (Some scope) ps in
  Scope.resolve_locals s b;
  compile c s b;
  Scope.upvalues s |> Hashtbl.iter (fun n (i, l) ->
    match l with
    | Scope.Upvalue -> add_opcode chunk (GetUpvalue i) |> ignore
    | Local -> add_opcode chunk (MakeCell i) |> ignore
    | _ -> ());
  add_opcode chunk (GetConstant (Closure {name = n; chunk = c; num_args = List.length ps; num_locals = Hashtbl.length (Scope.locals s) - List.length ps; upvalues = [||]} |> add_constant chunk)) |> ignore;
  if Scope.upvalues s |> Hashtbl.length > 0 then
    add_opcode chunk (Enclose (Scope.upvalues s |> Hashtbl.length)) |> ignore
| Call (f, a) ->
  List.iter (compile chunk scope) a;
  compile chunk scope f;
  add_opcode chunk (Call (List.length a)) |> ignore
| If bs ->
  let rec compile_if branches =
    (match branches with
    | [] -> failwith "Empty if"
    | (Ast.Bool true, t) :: _ -> compile chunk scope t
    | (c, t) :: bs' ->
      compile chunk scope c;
      let j = add_opcode chunk (JumpIfFalse 999) in
      compile chunk scope t;
      let j2 = add_opcode chunk (Jump 999) in
      let e = compile_if bs' in
      chunk.code.(j) <- JumpIfFalse (j2 + 1);
      chunk.code.(j2) <- Jump e);
    length chunk in
  compile_if bs |> ignore
| Struct (n, fs) -> add_opcode chunk (GetConstant (add_constant chunk (Type {name = n; fields = fs; methods = Hashtbl.create 16; traits = []}))) |> ignore
| Dot (e, f) ->
  compile chunk scope e;
  add_opcode chunk (Dot (add_name chunk f)) |> ignore
| Impl (t, ty, ms) ->
  compile chunk scope ty;
  List.iter (fun (n, ps, b) -> compile chunk scope (Fun (n, "self" :: ps, b)); add_opcode chunk (AddMethod (add_name chunk n)) |> ignore) ms;
  (match t with
  | None -> add_opcode chunk Pop
  | Some t' ->
    compile chunk scope t';
    add_opcode chunk Impl) |> ignore
| Trait (n, rs, ps) ->
  add_opcode chunk (GetConstant (add_constant chunk (Trait {name = n; requires = rs; provides = Hashtbl.create 16}))) |> ignore;
  List.iter (fun (n', ps, b) -> compile chunk scope (Fun (n', "self" :: ps, b)); add_opcode chunk (AddMethod (add_name chunk n')) |> ignore) ps
let to_closure chunk: t Value.closure = {name = ""; chunk; num_args = 0; num_locals = 0; upvalues = [||]}
let rec to_string chunk =
  "Code:\n" ^ (Array.mapi (fun i x -> string_of_int i ^ ": " ^ Opcode.to_string x) chunk.code |> Array.to_list |> String.concat "\n")
  ^ "\n\nConstants:\n" ^ (Array.mapi (fun i x -> string_of_int i ^ ": " ^ Value.to_string x) chunk.constants |> Array.to_list |> String.concat "\n")
  ^ "\n\nNames:\n" ^ (Array.mapi (fun i x -> string_of_int i ^ ": " ^ x) chunk.names |> Array.to_list |> String.concat "\n")