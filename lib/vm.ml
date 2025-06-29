type t = {
  mutable frames: frame list;
  globals: (string, Value.t) Hashtbl.t;
}
and frame = {
  closure: Value.closure;
  mutable ip: int;
  mutable stack: Value.t list;
  locals: Value.t array;
}

let make globals = {frames = []; globals}
let add_global vm value = Hashtbl.replace vm.globals value
let push vm value =
  let top_frame = List.hd vm.frames in
  top_frame.stack <- value :: top_frame.stack
let pop vm =
  let value = List.hd (List.hd vm.frames).stack in
  (List.hd vm.frames).stack <- List.tl (List.hd vm.frames).stack;
  value
let push_frame vm closure args = vm.frames <- {closure; ip = 0; stack = []; locals = Array.make closure.num_locals Types.Null |> Array.append (Array.of_list args)} :: vm.frames
let pop_frame vm =
  let frame = List.hd vm.frames in
  vm.frames <- List.tl vm.frames;
  frame
let subset a b = List.for_all (fun x -> List.mem x b) a
let step vm =
  let top_frame = List.hd vm.frames in
  if top_frame.ip < Chunk.length top_frame.closure.chunk then
    let continue = ref true in
    (match top_frame.closure.chunk.code.(let ip = top_frame.ip in top_frame.ip <- top_frame.ip + 1; ip) with
    | GetConstant i -> top_frame.closure.chunk.constants.(i) |> push vm
    | MakeList i ->
      let list = ref [] in
      for j = 1 to i do
        list := pop vm :: !list
      done;
      push vm (List !list)
    | MakeMap i ->
      let map = Hashtbl.create 16 in
      for j = 1 to i do
        let v = pop vm in
        Hashtbl.replace map (pop vm) v
      done;
      push vm (Map map)
    | GetGlobal i ->
      (match top_frame.closure.chunk.names.(i) |> Hashtbl.find_opt vm.globals with
      | None -> failwith ("Undefined global variable " ^ top_frame.closure.chunk.names.(i))
      | Some v -> push vm v)
    | SetGlobal i -> pop vm |> Hashtbl.replace vm.globals (top_frame.closure.chunk.names.(i))
    | GetLocal i ->
      (match top_frame.locals.(i) with
      | Cell c -> push vm !c
      | x -> push vm x)
    | SetLocal i ->
      (match top_frame.locals.(i) with
      | Cell c -> c := pop vm
      | _ -> top_frame.locals.(i) <- pop vm)
    | Call i ->
      let f = pop vm in
      let a = ref [] in
      for j = 1 to i do
        a := pop vm :: !a
      done;
      let rec call_helper func args =
        match func with
        | Types.Closure c ->
          if List.length args = c.num_args then
            push_frame vm c args
          else
            failwith (Value.to_string (Closure c) ^ " expected " ^ string_of_int c.num_args ^ " arguments, but received " ^ string_of_int i)
        | Primitive p -> p args |> push vm
        | Type t ->
          if List.length args = Hashtbl.length t.fields then
            Struct (t, args |> Array.of_list) |> push vm
          else
            failwith ("Constructor for " ^ Value.to_string (Type t) ^ " expected " ^ string_of_int (Hashtbl.length t.fields) ^ " arguments, but received " ^ string_of_int i)
        | Method (s, c) -> call_helper (Closure c) (s :: args)
        | x -> failwith ("Cannot call " ^ Value.to_string x) in
      call_helper f !a
    | MakeCell i ->
      (match top_frame.locals.(i) with
      | Cell _ -> ()
      | _ -> top_frame.locals.(i) <- Cell (ref top_frame.locals.(i)));
      push vm top_frame.locals.(i)
    | Enclose i ->
      let func = (match pop vm with Closure c -> c | x -> failwith ("Cannot add upvalues to " ^ Value.to_string x)) in
      let upvalues = ref [] in
      for j = 1 to i do
        upvalues := (match pop vm with Cell c -> c | x -> failwith ("Upvalue " ^ Value.to_string x ^ " is not a cell")) :: !upvalues
      done;
      push vm (Closure {func with upvalues = !upvalues |> Array.of_list})
    | GetUpvalue i -> Cell top_frame.closure.upvalues.(i) |> push vm
    | DerefUpvalue i -> !(top_frame.closure.upvalues.(i)) |> push vm
    | Jump i -> top_frame.ip <- i
    | JumpIfFalse i ->
      (match pop vm with
      | Null | Bool false | Number 0.0 | String "" | List [] -> top_frame.ip <- i
      | Map m when Hashtbl.length m = 0 -> top_frame.ip <- i
      | _ -> ())
    | Dot i -> top_frame.closure.chunk.names.(i) |> Value.dot (pop vm) |> push vm
    | AddMethod i ->
      let m = pop vm in
      Hashtbl.replace (match List.hd top_frame.stack with
      | Type t -> t.methods
      | Trait t -> t.provides
      | x -> failwith ("Cannot add methods to " ^ Value.to_string x)) top_frame.closure.chunk.names.(i) m
    | Impl ->
      let t = pop vm in
      (match (t, pop vm) with
      | (Trait t', Type ty') ->
        if not (List.mem t' ty'.traits) then
          (if subset t'.requires (ty'.methods |> Hashtbl.to_seq_keys |> List.of_seq) then
            ty'.traits <- t' :: ty'.traits
          else
            failwith (Value.to_string (Type ty') ^ " does not fully implement " ^ Value.to_string (Trait t') ^ ": missing " ^ String.concat ", " (List.filter (fun x -> not (ty'.methods |> Hashtbl.to_seq_keys |> List.of_seq |> List.mem x)) t'.requires)))
      | (Trait _, x) -> failwith ("Cannot implement for " ^ Value.to_string x ^ ": it is not a type")
      | (x, Type _) -> failwith ("Cannot implement " ^ Value.to_string x ^ ": it is not a trait")
      | _ -> failwith "How did you even get here?")
    | BaseTrait -> push vm (Trait Value.base_trait)
    | Import i ->
      let c = Chunk.empty () in
      let s = Scope.make None true [] in
      let b = Ast.Call (Fun ("", [], top_frame.closure.chunk.names.(i) |> open_in |> Lexing.from_channel |> Parser.program Lexer.read), []) in
      Scope.resolve_locals s b;
      Chunk.compile c s b;
      push_frame vm {name = ""; chunk = c; num_args = 0; num_locals = Hashtbl.length s.locals; upvalues = [||]} []
    | DupDot i -> top_frame.closure.chunk.names.(i) |> Value.dot (List.hd top_frame.stack) |> push vm
    | TailCall i ->
      let f = pop vm in
      let a = ref [] in
      for j = 1 to i do
        a := pop vm :: !a
      done;
      let rec tail_call_helper func args =
        match func with
        | Types.Closure c ->
          if List.length args = c.num_args then
            (pop_frame vm |> ignore;
            push_frame vm c args)
          else
            failwith (Value.to_string (Closure c) ^ " expected " ^ string_of_int c.num_args ^ " arguments, but received " ^ string_of_int i)
        | Primitive p -> p args |> push vm
        | Type t ->
          if List.length args = Hashtbl.length t.fields then
            Struct (t, args |> Array.of_list) |> push vm
          else
            failwith ("Constructor for " ^ Value.to_string (Type t) ^ " expected " ^ string_of_int (Hashtbl.length t.fields) ^ " arguments, but received " ^ string_of_int i)
        | Method (s, c) -> tail_call_helper (Closure c) (s :: args)
        | x -> failwith ("Cannot tail call " ^ Value.to_string x) in
      tail_call_helper f !a
    | Throw ->
      let rec find_handler ip = function
      | [] -> None
      | (s, e) :: hs ->
        if s <= ip && ip <= e then
          Some (e + 1)
        else
          find_handler ip hs in
      let e = pop vm in
      let found = ref false in
      while List.length vm.frames > 0 && not !found do
        let f = List.hd vm.frames in
        match find_handler f.ip f.closure.chunk.handlers with
        | None -> pop_frame vm |> ignore
        | Some ip ->
          found := true;
          f.ip <- ip;
          push vm e
      done;
      if not !found then
        (continue := false;
        prerr_endline ("Uncaught exception: " ^ Value.to_string e)));
    !continue
  else if List.length vm.frames = 1 then
    false
  else
    ((match (pop_frame vm).stack with
    | [] -> Types.Null
    | x :: _ -> x) |> push vm;
    true)
let call vm closure args =
  push_frame vm closure args;
  while step vm do () done;
  match (pop_frame vm).stack with
  | [] -> Types.Null
  | x :: _ -> x