type t = {
  mutable frames: frame list;
}
and frame = {
  closure: Value.closure;
  mutable ip: int;
  mutable stack: Value.t list;
  locals: Value.t array;
}

let make () = {frames = []}
let push vm value =
  let top_frame = List.hd vm.frames in
  top_frame.stack <- value :: top_frame.stack
let pop vm =
  let value = List.hd (List.hd vm.frames).stack in
  (List.hd vm.frames).stack <- List.tl (List.hd vm.frames).stack;
  value
let push_frame vm closure args = vm.frames <- {closure; ip = 0; stack = []; locals = Array.make closure.num_locals (Types.List []) |> Array.append (Array.of_list args)} :: vm.frames
let pop_frame vm =
  let frame = List.hd vm.frames in
  vm.frames <- List.tl vm.frames;
  frame
let throw vm error =
  let rec find_handler ip = function
  | [] -> None
  | (s, e) :: hs ->
    if s <= ip && ip <= e then
      Some (e + 1)
    else
      find_handler ip hs in
  let found = ref false in
  while List.length vm.frames > 0 && not !found do
    let f = List.hd vm.frames in
    match find_handler f.ip f.closure.chunk.handlers with
    | None -> pop_frame vm |> ignore
    | Some ip ->
      found := true;
      f.ip <- ip;
      push vm error
  done;
  (if not !found then
    prerr_endline ("Uncaught exception: " ^ Value.to_string error));
  !found
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
      (match Module.find top_frame.closure.module' top_frame.closure.chunk.names.(i) with
      | None -> failwith ("Undefined global variable " ^ top_frame.closure.chunk.names.(i))
      | Some v -> push vm v)
    | SetGlobal i -> Module.add top_frame.closure.module' top_frame.closure.chunk.names.(i) (pop vm)
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
        | Primitive p ->
          (try
            p args |> push vm
          with
          | _ -> failwith "Invalid arguments to primitive function")
        | Type t ->
          let Map map = List.hd args in
          (if map |> Hashtbl.to_seq_keys |> Seq.exists (fun (Types.String x) -> x |> Hashtbl.mem t.fields |> not) then
            failwith (Value.to_string (Type t) ^ " has no field(s) " ^ (map |> Hashtbl.to_seq_keys |> Seq.filter (fun (Types.String x) -> x |> Hashtbl.mem t.fields |> not) |> Seq.map (fun (Types.String x) -> x) |> List.of_seq |> String.concat ", ")));
          let fields = Array.make (Hashtbl.length t.fields) (Types.List []) in
          t.fields |> Hashtbl.to_seq |> Seq.iter (fun (n, i) -> Array.set fields i (match Hashtbl.find_opt map (String n) with
          | None ->
            (match t.default_values.(i) with
            | None -> failwith ("No default value for field " ^ n ^ " on " ^ Value.to_string (Type t))
            | Some v -> v)
          | Some v -> v));
          push vm (Struct (t, fields))
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
      | Bool false | Number 0.0 | String "" | List [] -> top_frame.ip <- i
      | Map m when Hashtbl.length m = 0 -> top_frame.ip <- i
      | _ -> ())
    | SetDefault i ->
      let v = pop vm in
      let Type t = pop vm in
      Array.set t.default_values (Hashtbl.find t.fields top_frame.closure.chunk.names.(i)) (Some v);
      push vm (Type t)
    | GetField i -> Value.get_field (pop vm) top_frame.closure.chunk.names.(i) |> push vm
    | SetField i ->
      let v = pop vm in
      Value.set_field (pop vm) top_frame.closure.chunk.names.(i) v
    | AddMethod i ->
      let m = pop vm in
      Hashtbl.replace (match List.hd top_frame.stack with
      | Type t -> t.methods
      | Trait t -> t.provides
      | x -> failwith ("Cannot add methods to " ^ Value.to_string x)) top_frame.closure.chunk.names.(i) m
    | Impl ->
      (match pop vm with
      | Trait t -> Value.impl t (pop vm)
      | x -> failwith ("Cannot implement " ^ Value.to_string x ^ ": it is not a trait"))
    | BaseTrait -> push vm (Trait Value.base_trait)
    | Import i ->
      let c = Chunk.empty () in
      let s = Scope.make None [] in
      let m = Module.make top_frame.closure.chunk.names.(i) [] in
      let b = top_frame.closure.chunk.names.(i) |> open_in |> Lexing.from_channel |> Parser.program Lexer.read in
      Chunk.compile c s m b;
      Chunk.add_opcode c PushModule |> ignore;
      push_frame vm (Chunk.to_closure c m) []
    | PushModule -> Module top_frame.closure.module' |> push vm
    | ImportFor i -> top_frame.closure.chunk.names.(i) |> Value.get_field (List.hd top_frame.stack) |> push vm
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
          let Map map = List.hd args in
          (if map |> Hashtbl.to_seq_keys |> Seq.exists (fun (Types.String x) -> x |> Hashtbl.mem t.fields |> not) then
            failwith (Value.to_string (Type t) ^ " has no field(s) " ^ (map |> Hashtbl.to_seq_keys |> Seq.filter (fun (Types.String x) -> x |> Hashtbl.mem t.fields |> not) |> Seq.map (fun (Types.String x) -> x) |> List.of_seq |> String.concat ", ")));
          let fields = Array.make (Hashtbl.length t.fields) (Types.List []) in
          t.fields |> Hashtbl.to_seq |> Seq.iter (fun (n, i) -> Array.set fields i (match Hashtbl.find_opt map (String n) with
          | None ->
            (match t.default_values.(i) with
            | None -> failwith ("No default value for field " ^ n ^ " on " ^ Value.to_string (Type t))
            | Some v -> v)
          | Some v -> v));
          push vm (Struct (t, fields))
        | Method (s, c) -> tail_call_helper (Closure c) (s :: args)
        | x -> failwith ("Cannot tail call " ^ Value.to_string x) in
      tail_call_helper f !a
    | Throw -> continue := throw vm (pop vm)
    | GetType -> Type (pop vm |> Value.type_of) |> push vm
    | Dup ->
      let v = pop vm in
      push vm v;
      push vm v);
    !continue
  else if List.length vm.frames = 1 then
    false
  else
    (let r = pop vm in
    pop_frame vm |> ignore;
    push vm r;
    true)
let call vm (closure: Types.closure) args =
  push_frame vm closure args;
  while step vm do () done;
  match vm.frames with
  | [] -> Types.List []
  | {stack = []} :: _ ->
    pop_frame vm |> ignore;
    List []
  | {stack = x :: _} :: _ ->
    pop_frame vm |> ignore;
    x