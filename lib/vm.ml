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
let push_frame vm chunk locals = vm.frames <- {chunk; ip = 0; stack = []; locals = Array.of_list locals} :: vm.frames
let pop_frame vm =
  let frame = List.hd vm.frames in
  vm.frames <- List.tl vm.frames;
  frame
let call vm (closure: Chunk.t Value.closure) args =
  push_frame vm closure.func.chunk args;
  let top_frame = List.hd vm.frames in
  while top_frame.ip < Chunk.length top_frame.chunk do
    match top_frame.chunk.code.(let ip = top_frame.ip in top_frame.ip <- top_frame.ip + 1; ip) with
    | GetConstant i -> push vm top_frame.chunk.constants.(i)
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
  done;
  match (pop_frame vm).stack with
  | [] -> Value.Null
  | x :: _ -> x