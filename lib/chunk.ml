type t = {
  mutable code: Opcode.t array;
  mutable constants: t Value.t array;
  mutable names: string array;
}

let rec find_first value = function
| [||] -> None
| a ->
  if a.(0) = value then
    Some 0
  else
    match Array.length a - 1 |> Array.sub a 1 |> find_first value with
    | None -> None
    | Some i -> Some (i + 1)
let add_opcode chunk opcode =
  chunk.code <- Array.append chunk.code [|opcode|];
  Array.length chunk.code - 1
let add_constant chunk constant =
  match find_first constant chunk.constants with
  | None ->
    chunk.constants <- Array.append chunk.constants [|constant|];
    Array.length chunk.constants - 1
  | Some i -> i
let length chunk = Array.length chunk.code
let rec compile chunk = function
| Ast.Block [] -> ()
| Block (x :: xs) ->
  compile chunk x;
  compile chunk (Block xs)
| Null -> add_opcode chunk (GetConstant (add_constant chunk Null)) |> ignore
| Bool b -> add_opcode chunk (GetConstant (add_constant chunk (Bool b))) |> ignore
| Number n -> add_opcode chunk (GetConstant (add_constant chunk (Number n))) |> ignore
| String s -> add_opcode chunk (GetConstant (add_constant chunk (String s))) |> ignore
| List l ->
  List.iter (compile chunk) l;
  add_opcode chunk (MakeList (List.length l)) |> ignore
| Map m ->
  List.iter (fun (k, v) -> compile chunk k; compile chunk v) m;
  add_opcode chunk (MakeMap (List.length m)) |> ignore
let to_string chunk =
  "Code:\n" ^ (Array.mapi (fun i x -> string_of_int i ^ ": " ^ Opcode.to_string x) chunk.code |> Array.to_list |> String.concat "\n")
  ^ "\n\nConstants:\n" ^ (Array.mapi (fun i x -> string_of_int i ^ ": " ^ Value.to_string x) chunk.constants |> Array.to_list |> String.concat "\n")
  ^ "\n\nNames:\n" ^ (Array.mapi (fun i x -> string_of_int i ^ ": " ^ x) chunk.names |> Array.to_list |> String.concat "\n")