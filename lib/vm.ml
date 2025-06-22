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
let push_frame vm (closure: Chunk.t Value.closure) args = vm.frames <- {chunk = closure.func.chunk; ip = 0; stack = []; locals = Array.make closure.func.num_locals Value.Null |> Array.append (Array.of_list args)} :: vm.frames
let pop_frame vm =
  let frame = List.hd vm.frames in
  vm.frames <- List.tl vm.frames;
  frame
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
      | None -> failwith ("Undefined global variable: " ^ Chunk.get_name top_frame.chunk i)
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
      let func = pop vm in
      let args' = ref [] in
      for j = 1 to i do
        args' := pop vm :: !args'
      done;
      (match func with
      | Closure c ->
        if i = c.func.num_args then
          call vm c !args' |> push vm
        else
          failwith ("Function expected " ^ string_of_int c.func.num_args ^ " arguments, but received " ^ string_of_int i)
      | Primitive p -> p !args' |> push vm
      | x -> failwith ("Cannot call " ^ Value.to_string x))
    | MakeCell i ->
      (match top_frame.locals.(i) with
      | Cell _ -> ()
      | _ -> top_frame.locals.(i) <- Cell (ref top_frame.locals.(i)));
      push vm top_frame.locals.(i)
    | MakeClosure i ->
      let func = (match pop vm with Function f -> f | _ -> failwith "Cannot make a closure out of this") in
      let upvalues = ref [] in
      for j = 1 to i do
        upvalues := (match pop vm with Cell c -> c | _ -> failwith "This upvalue should be a cell") :: !upvalues
      done;
      push vm (Closure {func; upvalues = Array.of_list !upvalues})
    | GetUpvalue i -> Cell closure.upvalues.(i) |> push vm
    | DerefUpvalue i -> !(closure.upvalues.(i)) |> push vm
    | Jump i -> top_frame.ip <- i
    | JumpIfFalse i ->
      (match pop vm with
      | Null | Bool false | Number 0.0 | String "" | List [] -> top_frame.ip <- i
      | Map m when Hashtbl.length m = 0 -> top_frame.ip <- i
      | _ -> ())
  done;
  match (pop_frame vm).stack with
  | [] -> Value.Null
  | x :: _ -> x