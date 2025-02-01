(* open OUnit2
open Oops

let eval_tests = "eval tests" >::: [
  "empty block" >:: (fun _ -> assert_equal (Eval.eval (EBlock []) (Env.create [] None) |> Value.repr) RNull);
  "block" >:: (fun _ -> assert_equal (Eval.eval (EBlock [EBool true; ENumber 4.0]) (Env.create [] None) |> Value.repr) (RNumber 4.0));
  "null" >:: (fun _ -> assert_equal (Eval.eval ENull (Env.create [] None) |> Value.repr) RNull);
  "bool" >:: (fun _ -> assert_equal (Eval.eval (EBool true) (Env.create [] None) |> Value.repr) (RBool true));
  "number" >:: (fun _ -> assert_equal (Eval.eval (ENumber 3.5) (Env.create [] None) |> Value.repr) (RNumber 3.5));
  "string" >:: (fun _ -> assert_equal (Eval.eval (EString "ab") (Env.create [] None) |> Value.repr) (RString "ab"));
  (* "list" >:: (fun _ -> assert_equal (Eval.eval (EList [EList []; ENumber 1.0]) (Env.create [] None) |> Value.repr) (RList [|RList [||]; |])); *)
]

let _ = run_test_tt_main eval_tests *)