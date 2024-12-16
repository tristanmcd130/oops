open Oops
open Exp
open Lexing

let rec repl (env: Env.t) (line_num: int): unit =
  Printf.printf "%d> " line_num;
  flush stdout;
  let result = Eval.eval (Parser.prog Lexer.read (from_channel stdin)) env in
  print_endline (Eval.string_of_exp result);
  Env.add env (Printf.sprintf "_%d" line_num) result;
  repl env (line_num + 1)

let global_env: Env.t = Env.make [
  ("print", Exp.make_primitive (fun [x] -> print_endline (Eval.string_of_exp x); Exp.make_null ()));
  ("List", EClass Exp.list_class);
] None;;

Eval.run_file "prelude.oops" global_env

let () = match Sys.argv with
| [|_|] -> repl global_env 1
| [|_; f|] -> ignore (Eval.run_file f global_env)