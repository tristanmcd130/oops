open Types

type t = module'

let globals = {filename = ""; vars = [
  ("print", Primitive (fun [x] -> x |> Value.to_string |> print_endline; Null));
  ("Base", Trait Value.base_trait);
  ("Null", Type Value.null_type);
  ("Bool", Type Value.bool_type);
  ("Number", Type Value.number_type);
  ("String", Type Value.string_type);
  ("List", Type Value.list_type);
  ("Map", Type Value.map_type);
  ("Function", Type Value.function_type);
  ("Type", Type Value.type_type);
  ("Trait", Type Value.trait_type);
  ("Module", Type Value.module_type);
] |> List.to_seq |> Hashtbl.of_seq; exports = []}
let make filename vars = {filename; vars = vars |> List.to_seq |> Hashtbl.of_seq; exports = []}
let export module' names = module'.exports <- names @ module'.exports
let find module' name =
  match Hashtbl.find_opt module'.vars name with
  | None -> Hashtbl.find_opt globals.vars name
  | Some v -> Some v
let add module' = Hashtbl.replace module'.vars