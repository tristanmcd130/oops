open Oops

let vm = Vm.make ()
let module' = Module.make "" []
let run_from_channel channel =
  let chunk = Chunk.empty () in
  channel |> Lexing.from_channel |> Parser.program Lexer.read |> Chunk.compile chunk (Scope.make None []) module';
  try
    Vm.call vm (Chunk.to_closure chunk module') []
  with
  | e ->
    prerr_endline ("Uncaught primitive exception: " ^ Printexc.to_string e ^ "\n" ^ Printexc.get_backtrace ());
    List []
let rec repl line_num =
  Printf.printf "%d> " line_num;
  flush stdout;
  let result = run_from_channel stdin in
  Module.add module' "_" result;
  Module.add module' (Printf.sprintf "_%d" line_num) result;
  result |> Value.to_string |> print_endline;
  repl (line_num + 1)
let () =
  Printexc.record_backtrace true;
  "prelude.oops" |> open_in |> run_from_channel |> ignore;
  match Sys.argv with
  | [|_|] -> repl 1
  | [|_; f|] -> f |> open_in |> run_from_channel |> ignore;
  | _ -> failwith ("Usage: " ^ Sys.argv.(0) ^ " [file]")