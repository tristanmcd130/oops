open Oops
open Lexing

let global_env = Env.create [
  ("print", Value.VPrimitive (fun [x] -> Eval.to_string x |> print_endline; VNull));
  ("List", VType Value.list_type);
  ("String", VType Value.string_type);
  ("Dict", VType Value.dict_type);
  ("Module", VType Value.module_type);
  ("Printable", VTrait Value.printable_trait);
  ("sqrt", VPrimitive (fun [VNumber x] -> VNumber (sqrt x)));
  ("type", VPrimitive (fun [x] -> VType (Value.type_of x)));
] None

let rec repl line_num =
  Printf.printf "%d> " line_num;
  flush stdout;
  (try
    let result = Eval.eval (from_channel stdin |> Parser.prog Lexer.read) global_env in
    if result <> VNull then
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