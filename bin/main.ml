open Oops
open Lexing

let global_env = Env.create [
  ("print", Value.VPrimitive (fun [x] -> Eval.to_string x |> print_endline; VNull))
] None

let rec repl line_num =
  Printf.printf "%d> " line_num;
  flush stdout;
  let result = Eval.eval (from_channel stdin |> Parser.prog Lexer.read) global_env in
  if result <> VNull then
    Eval.to_string result |> print_endline;
  Env.bind global_env (Printf.sprintf "_%d" line_num) result |> ignore;
  repl (line_num + 1)

let run_file filename = Eval.eval (In_channel.open_text filename |> from_channel |> Parser.prog Lexer.read) global_env |> ignore;;

let () =
  run_file "prelude.oops";
  match Sys.argv with
  | [|_|] -> repl 1
  | [|_; f|] -> run_file f
  | _ -> print_endline ("Usage: " ^ Sys.argv.(0) ^ " [file]")