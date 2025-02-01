open Oops
open Lexing

let rec repl env line_num =
  Printf.printf "%d> " line_num;
  flush stdout;
  let result = Eval.eval (from_channel stdin |> Parser.prog Lexer.read) env in
  Eval.to_string result |> print_endline;
  Env.bind env (Printf.sprintf "_%d" line_num) result;
  repl env (line_num + 1)

let global_env = Env.create [
  ("print", Value.make_primitive (fun [x] -> Eval.to_string x |> print_endline; Value.make_null ()))
] None
let () = repl global_env 1