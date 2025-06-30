open Types

type t = module'

let globals = {name = "globals"; parent = None; vars = [
  ("print", Primitive (fun [x] -> x |> Value.to_string |> print_endline; Null));
] |> List.to_seq |> Hashtbl.of_seq; exports = []}
let make name parent = {name; parent = Some (match parent with None -> globals | Some p -> p); vars = Hashtbl.create 16; exports = []}
let export module' names = module'.exports <- names @ module'.exports
let rec find module' name =
  match Hashtbl.find_opt module'.vars name with
  | None ->
    (match module'.parent with
    | None -> None
    | Some p -> find p name)
  | Some v -> Some v
let add module' = Hashtbl.replace module'.vars