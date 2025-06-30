type t = {
  parent: t option;
  locals: (string, int) Hashtbl.t;
  upvalues: (string, int * level) Hashtbl.t;
}
and level =
| Global
| Upvalue
| Local

let make parent locals = {parent; locals = List.mapi (fun i n -> (n, i)) locals |> List.to_seq |> Hashtbl.of_seq; upvalues = Hashtbl.create 16}
let rec get_upvalue_level scope name =
  match Hashtbl.find_opt scope.locals name with
  | None ->
    (match scope.parent with
    | None -> Global
    | Some p -> get_upvalue_level p name)
  | Some _ -> Upvalue
let get_level scope name =
  match Hashtbl.find_opt scope.locals name with
  | None ->
    (match scope.parent with
    | None -> Global
    | Some p -> get_upvalue_level p name)
  | Some _ -> Local
let find_local scope name = Hashtbl.find scope.locals name
let add_local scope name =
  match Hashtbl.find_opt scope.locals name with
  | None ->
    Hashtbl.length scope.locals |> Hashtbl.replace scope.locals name;
    Hashtbl.length scope.locals - 1
  | Some i -> i
let rec add_upvalue scope name =
  match Hashtbl.find_opt scope.upvalues name with
  | None ->
    (match scope.parent with
    | None -> failwith (name ^ " must be a global, no upvalues reference it")
    | Some p ->
      (match Hashtbl.find_opt p.locals name with
      | None ->
        let i = add_upvalue p name in
        Hashtbl.replace scope.upvalues name (i, Upvalue)
      | Some i -> Hashtbl.replace scope.upvalues name (i, Local));
      Hashtbl.length scope.upvalues - 1)
  | Some (i, _) -> i
let rec resolve_locals scope = function
| Ast.Block bs -> List.iter (resolve_locals scope) bs
| Assign (n, _) -> add_local scope n |> ignore
| _ -> ()