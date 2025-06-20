open Oops

let vm = Vm.make ([] |> List.to_seq |> Hashtbl.of_seq)
let run_from_channel channel =
  let chunk: Chunk.t = {code = [||]; constants = [||]; names = [||]} in
  channel |> Lexing.from_channel |> Parser.program Lexer.read |> Chunk.compile chunk;
  Vm.call vm {func = {chunk; num_args = 0; num_locals = 0}; upvalues = [||]} []
let rec repl line_num =
  Printf.printf "%d> " line_num;
  flush stdout;
  let result = run_from_channel stdin in
  Vm.add_global vm (Printf.sprintf "_%d" line_num) result;
  result |> Value.to_string |> print_endline;
  repl (line_num + 1)
let () =
  match Sys.argv with
  | [|_|] -> repl 1
  | [|_; f|] -> f |> open_in |> run_from_channel |> ignore;
  | _ -> failwith ("Usage: " ^ Sys.argv.(0) ^ " [file]")