type t = {
  parent: t option;
  locals: (string, int) Hashtbl.t;
  upvalues: (string, int * level) Hashtbl.t;
}
and level =
| Global
| Upvalue
| Local

let make parent = {parent; locals = Hashtbl.create 16; upvalues = Hashtbl.create 16}
let parent scope = scope.parent
let locals scope = scope.locals
let upvalues scope = scope.upvalues
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
      match Hashtbl.find_opt p.locals name with
      | None ->
        let i = add_upvalue p name in
        Hashtbl.replace scope.upvalues name (i, Upvalue);
        i
      | Some i ->
        Hashtbl.replace scope.upvalues name (i, Local);
        i)
  | Some (i, _) -> i