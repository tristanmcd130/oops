open OUnit2
open Oops

let make_parse_test string exp = fun _ -> assert_equal (string |> Lexing.from_string |> Parser.prog Lexer.read) exp
let parse_tests = "parse tests" >::: [
  "null" >:: make_parse_test "null" ENull;
  "bool" >:: make_parse_test "true false" (EBlock [EBool true; EBool false]);
  "number" >:: make_parse_test "-5.6" (ENumber (-5.6));
  "string" >:: make_parse_test "\"hello world\\n\"" (EString "hello world\n");
  "list" >:: make_parse_test "[1, true, null, []]" (EList [ENumber 1.0; EBool true; ENull; EList []]);
  "dict" >:: make_parse_test "{1: 2, null: {}}" (EDict [(ENumber 1.0, ENumber 2.0); (ENull, EDict [])]);
  "fun" >:: make_parse_test "fun(x) end" (EFun (["x"], EBlock []));
  "var" >:: make_parse_test "x" (EVar "x");
  "dot" >:: make_parse_test "x.y" (EDot (EVar "x", "y"));
  "call" >:: make_parse_test "x(1, \"\")" (ECall (EVar "x", [ENumber 1.0; EString ""]));
  "not" >:: make_parse_test "not true" (ECall (EDot (EBool true, "not"), []));
  "negate" >:: make_parse_test "-true" (ECall (EDot (EBool true, "u-"), []));
  "or" >:: make_parse_test "-true or false" (ECall (EDot (ECall (EDot (EBool true, "u-"), []), "or"), [EBool false]));
  "and" >:: make_parse_test "true and false" (ECall (EDot (EBool true, "and"), [EBool false]));
  "<" >:: make_parse_test "0 < 1" (ECall (EDot (ENumber 0.0, "<"), [ENumber 1.0]));
  "<=" >:: make_parse_test "0 <= 1" (ECall (EDot (ENumber 0.0, "<="), [ENumber 1.0]));
  "==" >:: make_parse_test "0 == 1" (ECall (EDot (ENumber 0.0, "=="), [ENumber 1.0]));
  "!=" >:: make_parse_test "0 != 1" (ECall (EDot (ENumber 0.0, "!="), [ENumber 1.0]));
  ">" >:: make_parse_test "0 > 1" (ECall (EDot (ENumber 0.0, ">"), [ENumber 1.0]));
  ">=" >:: make_parse_test "0 >= 1" (ECall (EDot (ENumber 0.0, ">="), [ENumber 1.0]));
  "::" >:: make_parse_test "1 :: 2 :: 3 :: []" (ECall (EDot (ECall (EDot (ECall (EDot (EList [], "::"), [ENumber 3.0]), "::"), [ENumber 2.0]), "::"), [ENumber 1.0]));
  "+" >:: make_parse_test "0 + 1" (ECall (EDot (ENumber 0.0, "+"), [ENumber 1.0]));
  "-" >:: make_parse_test "0 - 1" (ECall (EDot (ENumber 0.0, "-"), [ENumber 1.0]));
  "*" >:: make_parse_test "0 * 1" (ECall (EDot (ENumber 0.0, "*"), [ENumber 1.0]));
  "/" >:: make_parse_test "0 / 1" (ECall (EDot (ENumber 0.0, "/"), [ENumber 1.0]));
  "%" >:: make_parse_test "0 % 1" (ECall (EDot (ENumber 0.0, "%"), [ENumber 1.0]));
  "if" >:: make_parse_test "if a then b else c end" (EIf (EVar "a", EVar "b", EVar "c"));
  "cond" >:: make_parse_test "cond | false -> 1 | true -> 2 | true -> 3 end" (ECond [(EBool false, ENumber 1.0); (EBool true, ENumber 2.0); (EBool true, ENumber 3.0)]);
  "match" >:: make_parse_test "match x | a(b, c) -> 1 | a :: b -> 2 | _ -> 3 end" (EMatch (EVar "x", [(ECall (EVar "a", [EVar "b"; EVar "c"]), ENumber 1.0); (ECall (EDot (EVar "b", "::"), [EVar "a"]), ENumber 2.0); (EVar "_", ENumber 3.0)]));
  "let" >:: make_parse_test "let x = 5 in x end" (ELet ([(EVar "x", ENumber 5.0)], EVar "x"));
  "try" >:: make_parse_test "try x catch | _ -> 2 end" (ETry (EVar "x", [(EVar "_", ENumber 2.0)]));
  "parens" >:: make_parse_test "((((((((((((x))))) + 2)))))))" (ECall (EDot (EVar "x", "+"), [ENumber 2.0]));
  "assign" >:: make_parse_test "x = 5" (EAssign (EVar "x", ENumber 5.0));
  "assign pattern match" >:: make_parse_test "[x, y] = [4, 5]" (EAssign (EList [EVar "x"; EVar "y"], EList [ENumber 4.0; ENumber 5.0]));
  "dot assign" >:: make_parse_test "x.y = 4" (EDotAssign (EVar "x", "y", ENumber 4.0));
  "def" >:: make_parse_test "def f(x, y) x + y 3 end" (EDef ("f", ["x"; "y"], EBlock [ECall (EDot (EVar "x", "+"), [EVar "y"]); ENumber 3.0]));
  "struct" >:: make_parse_test "struct S a b end" (EStruct ("S", ["a"; "b"]));
  "trait" >:: make_parse_test "trait T a b def c(d) d end end" (ETrait ("T", ["a"; "b"], [("c", ["d"], EVar "d")]));
  "impl nothing" >:: make_parse_test "impl for S def a() end end" (EImpl (None, EVar "S", [("a", [], EBlock [])]));
  "impl" >:: make_parse_test "impl T for S def a() end end" (EImpl (Some (EVar "T"), EVar "S", [("a", [], EBlock [])]));
  "module" >:: make_parse_test "module M exports a a = 5 end" (EModule ("M", ["a"], EAssign (EVar "a", ENumber 5.0)));
  "import" >:: make_parse_test "import \"abc\"" (EImport "abc");
  "throw" >:: make_parse_test "throw a(b)" (EThrow (ECall (EVar "a", [EVar "b"])));
]

let make_eval_test exp value ?(env = Oops.Env.create [] None) = fun _ -> assert_equal (Oops.Eval.eval exp env) value
let eval_tests = "eval tests" >::: [
  "empty block" >:: make_eval_test (EBlock []) VNull;
  "null" >:: make_eval_test ENull VNull;
  "bool" >:: make_eval_test (EBool true) (VBool true);
  "number" >:: make_eval_test (ENumber (-5.67)) (VNumber (-5.67));
  "string" >:: make_eval_test (EString "goodbye cruel world") (VString "goodbye cruel world");
  "list" >:: make_eval_test (EList [ENull; ENumber 1.0; EString "a"]) (VList [VNull; VNumber 1.0; VString "a"]);
  "dict" >:: make_eval_test (EDict [(ENumber 1.0, EString "a"); (EString "b", ENumber 2.0)]) (VDict ([(Value.VNumber 1.0, Value.VString "a"); (VString "b", VNumber 2.0)] |> List.to_seq |> Hashtbl.of_seq));
  "fun" >:: make_eval_test (EFun (["a"; "b"], EBlock [EVar "a"; EVar "b"])) (VFunction ("", ["a"; "b"], EBlock [EVar "a"; EVar "b"], Env.create [] None));
  "var" >:: make_eval_test (EVar "x") (VNumber 2.3) ~env: (Env.create [("x", Value.VNumber 2.3)] None);
  "dot" >:: make_eval_test (EDot (EVar "x", "a")) (VNumber 5.0) ~env: (Env.create [("x", Value.VStruct (Oops.Value.make_type "S" ["a"; "b"], [("a", Value.VNumber 5.0); ("b", VString "")] |> List.to_seq |> Hashtbl.of_seq))] None);
  "call" >:: make_eval_test (ECall (EVar "f", [ENumber 5.0])) (VNumber 6.0) ~env: (Env.create [("f", Value.VPrimitive (fun [VNumber x] -> VNumber (x +. 1.0)))] None);
  "not" >:: make_eval_test (ECall (EDot (EBool true, "not"), [])) (VBool false);
  "negate" >:: make_eval_test (ECall (EDot (ENumber 6.5, "u-"), [])) (VNumber (-6.5));
  "or" >:: make_eval_test (ECall (EDot (EBool false, "or"), [EBool true])) (VBool true);
  "and" >:: make_eval_test (ECall (EDot (EBool false, "and"), [EBool true])) (VBool false);
  "<" >:: make_eval_test (ECall (EDot (ENumber 0.0, "<"), [ENumber 0.1])) (VBool true);
  "<=" >:: make_eval_test (ECall (EDot (ENumber 0.0, "<="), [ENumber 0.1])) (VBool true);
  "==" >:: make_eval_test (ECall (EDot (ENumber 0.0, "=="), [ENumber 0.1])) (VBool false);
  "!=" >:: make_eval_test (ECall (EDot (ENumber 0.0, "!="), [ENumber 0.1])) (VBool true);
  ">" >:: make_eval_test (ECall (EDot (ENumber 0.0, ">"), [ENumber 0.1])) (VBool false);
  ">=" >:: make_eval_test (ECall (EDot (ENumber 0.0, ">="), [ENumber 0.1])) (VBool false);
  "::" >:: make_eval_test (ECall (EDot (ECall (EDot (ECall (EDot (EList [], "::"), [ENumber 3.0]), "::"), [ENumber 2.0]), "::"), [ENumber 1.0])) (VList [VNumber 1.0; VNumber 2.0; VNumber 3.0]);
  "+" >:: make_eval_test (ECall (EDot (ENumber 1.0, "+"), [ENumber 3.0])) (VNumber 4.0);
  "-" >:: make_eval_test (ECall (EDot (ENumber 1.0, "-"), [ENumber 3.0])) (VNumber (-2.0));
  "*" >:: make_eval_test (ECall (EDot (ENumber 1.0, "*"), [ENumber 3.0])) (VNumber (3.0));
  "/" >:: make_eval_test (ECall (EDot (ENumber 1.0, "/"), [ENumber 4.0])) (VNumber 0.25);
  "%" >:: make_eval_test (ECall (EDot (ENumber 8.0, "%"), [ENumber 3.0])) (VNumber 2.0);
  "if" >:: make_eval_test (EIf (EBool true, ENumber 1.0, ENumber 2.0)) (VNumber 1.0);
  "cond" >:: make_eval_test (ECond [(EBool false, ENumber 1.0); (EBool true, ENumber 2.0); (EBool true, ENumber 3.0)]) (VNumber 2.0);
  "match literal" >:: make_eval_test (EMatch (ENumber 1.0, [(ENumber 2.0, EString "a"); (ENumber 1.0, EString "b")])) (VString "b");
  "match" >:: make_eval_test (EMatch (EList [ENumber 1.0; ENumber 2.0], [(ECall (EDot (EVar "y", "::"), [EVar "x"]), EVar "y")])) (VList [VNumber 2.0]);
  "let" >:: make_eval_test (ELet ([EVar "x", ENumber 3.5], EVar "x")) (VNumber 3.5);
  "assign" >:: make_eval_test (EBlock [EAssign (EVar "x", ENumber 5.0); EVar "x"]) (VNumber 5.0);
  "dot assign" >:: make_eval_test (EBlock [EDotAssign (EVar "x", "a", ENumber 5.0); EDot (EVar "x", "a")]) (VNumber 5.0) ~env: (Env.create [("x", Value.VStruct (Oops.Value.make_type "S" ["a"; "b"], [("a", Value.VNumber 5.0); ("b", VString "")] |> List.to_seq |> Hashtbl.of_seq))] None);
  "def" >:: make_eval_test (EBlock [EDef ("f", ["x"], ENumber 4.5); ECall (EVar "f", [ENumber 0.0])]) (VNumber 4.5);
  "struct" >:: make_eval_test (EBlock [EStruct ("S", ["a"; "b"]); EDot (ECall (EVar "S", [ENumber 1.0; EString "a"]), "b")]) (VString "a");
  "impl" >:: make_eval_test (EBlock [ETrait ("T", ["f"; "g"], [("h", [], EBool false)]); EStruct ("S", ["a"; "b"]); EImpl (Some (EVar "T"), EVar "S", [("f", [], ENull); ("g", [], ENull)]); ECall (EDot (ECall (EVar "S", [ENull; ENull]), "h"), [])]) (VBool false);
]

let _ =
  run_test_tt_main parse_tests;
  run_test_tt_main eval_tests