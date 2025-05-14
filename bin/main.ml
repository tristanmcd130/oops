open Oops
open Lexing

let global_env = Env.create [
  ("print", Value.Primitive (fun [x] -> Eval.to_string x |> print_endline; Null));
  ("Null", Type Value.null_type);
  ("Bool", Type Value.bool_type);
  ("Number", Type Value.number_type);
  ("String", Type Value.string_type);
  ("List", Type Value.list_type);
  ("Dict", Type Value.dict_type);
  ("Function", Type Value.function_type);
  ("Type", Type Value.type_type);
  ("Trait", Type Value.trait_type);
  ("Module", Type Value.module_type);
  ("Base", Trait Value.base_trait);
  ("Printable", Trait Value.printable_trait);
  ("sqrt", Primitive (fun [Number x] -> Number (sqrt x)));
  ("max", Primitive (fun [Number x; Number y] -> Number (max x y)));
] None

let rec repl line_num =
  Printf.printf "%d> " line_num;
  flush stdout;
  (try
    let result = Eval.eval (from_channel stdin |> Parser.prog Lexer.read) global_env in
    if result <> Null then
      Eval.to_string result |> print_endline;
    Env.bind global_env "_" result;
    Env.bind global_env ("_" ^ string_of_int line_num) result |> ignore
  with
  | Value.Runtime_error e -> print_endline ("Uncaught error: " ^ Eval.to_string e)
  | e -> print_endline ("Uncaught primitive error: " ^ Printexc.to_string e));
  repl (line_num + 1)

let () =
  Eval.run_file "prelude.oops" global_env;
  match Sys.argv with
  | [|_|] -> repl 1
  | [|_; f|] -> Eval.run_file f global_env
  | _ -> print_endline ("Usage: " ^ Sys.argv.(0) ^ " [file]")