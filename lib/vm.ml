type t = {
  mutable frames: frame list;
  globals: (string, Chunk.t Value.t) Hashtbl.t;
}
and frame = {
  chunk: Chunk.t;
  mutable ip: int;
  mutable stack: Chunk.t Value.t list;
  locals: Chunk.t Value.t array;
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
let push_frame vm (closure: Chunk.t Value.closure) args = vm.frames <- {chunk = closure.chunk; ip = 0; stack = []; locals = Array.make closure.num_locals Value.Null |> Array.append (Array.of_list args)} :: vm.frames
let pop_frame vm =
  let frame = List.hd vm.frames in
  vm.frames <- List.tl vm.frames;
  frame
let subset a b = List.for_all (fun x -> List.mem x b) a

(* TODO: figure out how to get this stuff into value.ml *)
let null_type: Chunk.t Value.typ = {name = "Null"; fields = []; methods = Hashtbl.create 16; traits = []}
let bool_type: Chunk.t Value.typ = {name = "Bool"; fields = []; methods = Hashtbl.create 16; traits = []}
let number_type: Chunk.t Value.typ = {name = "Number"; fields = []; methods = Hashtbl.create 16; traits = []}
let string_type: Chunk.t Value.typ = {name = "String"; fields = []; methods = Hashtbl.create 16; traits = []}
let list_type: Chunk.t Value.typ = {name = "List"; fields = []; methods = Hashtbl.create 16; traits = []}
let map_type: Chunk.t Value.typ = {name = "Map"; fields = []; methods = Hashtbl.create 16; traits = []}
let function_type: Chunk.t Value.typ = {name = "Function"; fields = []; methods = Hashtbl.create 16; traits = []}
let type_type: Chunk.t Value.typ = {name = "Type"; fields = []; methods = Hashtbl.create 16; traits = []}
let trait_type: Chunk.t Value.typ = {name = "Trait"; fields = []; methods = Hashtbl.create 16; traits = []}
let rec type_of = function
| Value.Null -> null_type
| Bool _ -> bool_type
| Number _ -> number_type
| String _ -> string_type
| List _ -> list_type
| Map _ -> map_type
| Closure _ | Primitive _ | Method _ -> function_type
| Cell c -> type_of !c
| Struct (t, _) -> t
| Type _ -> type_type
| Trait _ -> trait_type
let bind_self self = function
| Value.Closure c -> Value.Method {closure = c; self}
| Primitive (n, p) -> Primitive (n, fun args -> p (self :: args))
| x -> failwith ("Cannot bind self in " ^ Value.to_string x)
let rec get_method_from_traits (obj: Chunk.t Value.t) (name: string): Chunk.t Value.trait list -> Chunk.t Value.t option = function
| [] -> None
| t :: ts ->
  match Hashtbl.find_opt t.provides name with
  | None -> get_method_from_traits obj name ts
  | Some m -> Some m
let get_method obj name =
  (match Hashtbl.find_opt (type_of obj).methods name with
  | None ->
    (match get_method_from_traits obj name (type_of obj).traits with
    | None -> failwith ((type_of obj).name ^ " has no field/method " ^ name)
    | Some m -> m)
  | Some m -> m) |> bind_self obj
let dot obj name =
  match obj with
  | Value.Struct (_, fs) ->
    (match Hashtbl.find_opt fs name with
    | None -> get_method obj name
    | Some f -> f)
  | _ -> get_method obj name

let rec call vm closure args =
  push_frame vm closure args;
  (* closure.func.chunk |> Chunk.to_string |> print_endline; *)
  let top_frame = List.hd vm.frames in
  while top_frame.ip < Chunk.length top_frame.chunk do
    match Chunk.get_opcode top_frame.chunk (let ip = top_frame.ip in top_frame.ip <- top_frame.ip + 1; ip) with
    | GetConstant i -> Chunk.get_constant top_frame.chunk i |> push vm
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
      (match i |> Chunk.get_name top_frame.chunk |> Hashtbl.find_opt vm.globals with
      | None -> failwith ("Undefined global variable " ^ Chunk.get_name top_frame.chunk i)
      | Some v -> push vm v)
    | SetGlobal i -> pop vm |> Hashtbl.replace vm.globals (Chunk.get_name top_frame.chunk i)
    | Negate ->
      (match pop vm with
      | Number n -> push vm (Number (-.n))
      | _ -> failwith "Invalid argument to u-")
    | Add ->
      let r = pop vm in
      (match (pop vm, r) with
      | (Number n, Number n') -> push vm (Number (n +. n'))
      | (String s, String s') -> push vm (String (s ^ s'))
      | _ -> failwith "Invalid arguments to +")
    | Subtract ->
      let r = pop vm in
      (match (pop vm, r) with
      | (Number n, Number n') -> push vm (Number (n -. n'))
      | _ -> failwith "Invalid arguments to -")
    | Multiply ->
      let r = pop vm in
      (match (pop vm, r) with
      | (Number n, Number n') -> push vm (Number (n *. n'))
      | _ -> failwith "Invalid arguments to *")
    | Divide ->
      let r = pop vm in
      (match (pop vm, r) with
      | (Number n, Number n') -> push vm (Number (n /. n'))
      | _ -> failwith "Invalid arguments to /")
    | Modulo ->
      let r = pop vm in
      (match (pop vm, r) with
      | (Number n, Number n') -> push vm (Number (mod_float n n'))
      | _ -> failwith "Invalid arguments to %")
    | LT ->
      let r = pop vm in
      (match (pop vm, r) with
      | (Number n, Number n') -> push vm (Bool (n < n'))
      | _ -> failwith "Invalid arguments to <")
    | LE ->
      let r = pop vm in
      (match (pop vm, r) with
      | (Number n, Number n') -> push vm (Bool (n <= n'))
      | _ -> failwith "Invalid arguments to <=")
    | EQ ->
      let r = pop vm in
      push vm (Bool (pop vm = r))
    | NE ->
      let r = pop vm in
      push vm (Bool (pop vm <> r))
    | GT ->
      let r = pop vm in
      (match (pop vm, r) with
      | (Number n, Number n') -> push vm (Bool (n > n'))
      | _ -> failwith "Invalid arguments to >")
    | GE ->
      let r = pop vm in
      (match (pop vm, r) with
      | (Number n, Number n') -> push vm (Bool (n >= n'))
      | _ -> failwith "Invalid arguments to >=")
    | And ->
      let r = pop vm in
      (match (pop vm, r) with
      | (Bool b, Bool b') -> push vm (Bool (b && b'))
      | _ -> failwith "Invalid arguments to and")
    | Or ->
      let r = pop vm in
      (match (pop vm, r) with
      | (Bool b, Bool b') -> push vm (Bool (b || b'))
      | _ -> failwith "Invalid arguments to or")
    | Not ->
      (match pop vm with
      | Bool b -> push vm (Bool (not b))
      | _ -> failwith "Invalid argument to not")
    | Cons ->
      let r = pop vm in
      (match (pop vm, r) with
      | (x, List xs) -> push vm (List (x :: xs))
      | _ -> failwith "Invalid arguments to ::")
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
      let rec call_helper func args' =
        match func with
        | Value.Closure c ->
          if List.length args' = c.num_args then
            call vm c args' |> push vm
          else
            failwith ("Function " ^ c.name ^ " expected " ^ string_of_int c.num_args ^ " arguments, but received " ^ string_of_int i)
        | Primitive (_, p) -> p args' |> push vm
        | Type t ->
          if List.length args' = List.length t.fields then
            Struct (t, List.combine t.fields args' |> List.to_seq |> Hashtbl.of_seq) |> push vm
          else
            failwith (t.name ^ "'s constructor expected " ^ string_of_int (List.length t.fields) ^ " arguments, but received " ^ string_of_int i)
        | Method m -> call_helper (Closure m.closure) (m.self :: args')
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
    | GetUpvalue i -> Cell closure.upvalues.(i) |> push vm
    | DerefUpvalue i -> !(closure.upvalues.(i)) |> push vm
    | Jump i -> top_frame.ip <- i
    | JumpIfFalse i ->
      (match pop vm with
      | Null | Bool false | Number 0.0 | String "" | List [] -> top_frame.ip <- i
      | Map m when Hashtbl.length m = 0 -> top_frame.ip <- i
      | _ -> ())
    | Dot i -> Chunk.get_name top_frame.chunk i |> dot (pop vm) |> push vm
    | AddMethod i ->
      let m = pop vm in
      Hashtbl.replace (match List.hd top_frame.stack with
      | Type t -> t.methods
      | Trait t -> t.provides
      | x -> failwith ("Cannot add methods to " ^ Value.to_string x)) (Chunk.get_name top_frame.chunk i) m
    | Impl ->
      let t = pop vm in
      (match (t, pop vm) with
      | (Trait t', Type ty') ->
        if not (List.mem t' ty'.traits) then
          (if subset t'.requires (ty'.methods |> Hashtbl.to_seq_keys |> List.of_seq) then
            ty'.traits <- t' :: ty'.traits
          else
            failwith (ty'.name ^ " does not fully implement " ^ t'.name ^ ": missing " ^ String.concat ", " (List.filter (fun x -> not (ty'.methods |> Hashtbl.to_seq_keys |> List.of_seq |> List.mem x)) t'.requires)))
      | (Trait _, x) -> failwith ("Cannot implement for " ^ Value.to_string x ^ ": it is not a type")
      | (x, Type _) -> failwith ("Cannot implement " ^ Value.to_string x ^ ": it is not a trait")
      | _ -> failwith "How did you even get here?")
    | Pop -> pop vm |> ignore
  done;
  match (pop_frame vm).stack with
  | [] -> Value.Null
  | x :: _ -> x