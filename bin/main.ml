open Oops
open Exp
open Lexing

let rec repl (env: Env.t): unit =
  print_string "> ";
  flush stdout;
  print_endline (Eval.string_of_exp (Eval.eval (Parser.prog Lexer.read (from_channel stdin)) env));
  repl env

let global_env: Env.t = Env.make [
  ("print", Exp.make_primitive (fun [x] -> print_endline (Eval.string_of_exp x); Exp.make_null ()));
] None

let () = match Sys.argv with
| [|_|] -> repl global_env
| [|_; f|] -> ignore (Eval.eval (Parser.prog Lexer.read (from_channel (In_channel.open_text f))) global_env)