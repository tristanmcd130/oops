open OUnit2
open Oops

let make_parse_test string exp = fun _ -> assert_equal (string |> Lexing.from_string |> Parser.prog Lexer.read) exp
let parse_tests = "parse tests" >::: [
  "null" >:: make_parse_test "null" Null;
  "bool" >:: make_parse_test "true false" (Block [Bool true; Bool false]);
  "number" >:: make_parse_test "-5.6" (Number (-5.6));
  "string" >:: make_parse_test "\"hello world\\n\"" (String "hello world\n");
  "list" >:: make_parse_test "[1, true, null, []]" (List [Number 1.0; Bool true; Null; List []]);
  "dict" >:: make_parse_test "{1: 2, null: {}}" (Dict [(Number 1.0, Number 2.0); (Null, Dict [])]);
  "fun" >:: make_parse_test "fun(x) end" (Fun (["x"], Block []));
  "var" >:: make_parse_test "x" (Var "x");
  "dot" >:: make_parse_test "x.y" (Dot (Var "x", "y"));
  "call" >:: make_parse_test "x(1, \"\")" (Call (Var "x", [Number 1.0; String ""]));
  "not" >:: make_parse_test "not true" (Call (Dot (Bool true, "not"), []));
  "negate" >:: make_parse_test "-true" (Call (Dot (Bool true, "u-"), []));
  "or" >:: make_parse_test "-true or false" (Call (Dot (Call (Dot (Bool true, "u-"), []), "or"), [Bool false]));
  "and" >:: make_parse_test "true and false" (Call (Dot (Bool true, "and"), [Bool false]));
  "<" >:: make_parse_test "0 < 1" (Call (Dot (Number 0.0, "<"), [Number 1.0]));
  "<=" >:: make_parse_test "0 <= 1" (Call (Dot (Number 0.0, "<="), [Number 1.0]));
  "==" >:: make_parse_test "0 == 1" (Call (Dot (Number 0.0, "=="), [Number 1.0]));
  "!=" >:: make_parse_test "0 != 1" (Call (Dot (Number 0.0, "!="), [Number 1.0]));
  ">" >:: make_parse_test "0 > 1" (Call (Dot (Number 0.0, ">"), [Number 1.0]));
  ">=" >:: make_parse_test "0 >= 1" (Call (Dot (Number 0.0, ">="), [Number 1.0]));
  "::" >:: make_parse_test "1 :: 2 :: 3 :: []" (Call (Dot (Call (Dot (Call (Dot (List [], "::"), [Number 3.0]), "::"), [Number 2.0]), "::"), [Number 1.0]));
  "+" >:: make_parse_test "0 + 1" (Call (Dot (Number 0.0, "+"), [Number 1.0]));
  "-" >:: make_parse_test "0 - 1" (Call (Dot (Number 0.0, "-"), [Number 1.0]));
  "*" >:: make_parse_test "0 * 1" (Call (Dot (Number 0.0, "*"), [Number 1.0]));
  "/" >:: make_parse_test "0 / 1" (Call (Dot (Number 0.0, "/"), [Number 1.0]));
  "%" >:: make_parse_test "0 % 1" (Call (Dot (Number 0.0, "%"), [Number 1.0]));
  "if" >:: make_parse_test "if a then b else c end" (If [(Var "a", Var "b"); (Bool true, Var "c")]);
  "elseif" >:: make_parse_test "if false then 1 elseif true then 2 else 3 end" (If [(Bool false, Number 1.0); (Bool true, Number 2.0); (Bool true, Number 3.0)]);
  "match" >:: make_parse_test "match x | a(b, c) -> 1 | a :: b -> 2 | _ -> 3 end" (Match (Var "x", [(Call (Var "a", [Var "b"; Var "c"]), Number 1.0); (Call (Dot (Var "b", "::"), [Var "a"]), Number 2.0); (Var "_", Number 3.0)]));
  "let" >:: make_parse_test "let x = 5 in x end" (Let ([(Var "x", Number 5.0)], Var "x"));
  "try" >:: make_parse_test "try x catch | _ -> 2 end" (Try (Var "x", [(Var "_", Number 2.0)]));
  "parens" >:: make_parse_test "((((((((((((x))))) + 2)))))))" (Call (Dot (Var "x", "+"), [Number 2.0]));
  "assign" >:: make_parse_test "x = 5" (Assign (Var "x", Number 5.0));
  "assign pattern match" >:: make_parse_test "[x, y] = [4, 5]" (Assign (List [Var "x"; Var "y"], List [Number 4.0; Number 5.0]));
  "dot assign" >:: make_parse_test "x.y = 4" (DotAssign (Var "x", "y", Number 4.0));
  "def" >:: make_parse_test "def f(x, y) x + y 3 end" (Def ("f", ["x"; "y"], Block [Call (Dot (Var "x", "+"), [Var "y"]); Number 3.0]));
  "struct" >:: make_parse_test "struct S a b end" (Struct ("S", ["a"; "b"]));
  "trait" >:: make_parse_test "trait T a b def c(d) d end end" (Trait ("T", ["a"; "b"], [("c", ["d"], Var "d")]));
  "impl nothing" >:: make_parse_test "impl for S def a() end end" (Impl (None, Var "S", [("a", [], Block [])]));
  "impl" >:: make_parse_test "impl T for S def a() end end" (Impl (Some (Var "T"), Var "S", [("a", [], Block [])]));
  "module" >:: make_parse_test "module M exports a a = 5 end" (Module ("M", ["a"], Assign (Var "a", Number 5.0)));
  "import" >:: make_parse_test "import \"abc\"" (Import "abc");
  "throw" >:: make_parse_test "throw a(b)" (Throw (Call (Var "a", [Var "b"])));
]

let make_eval_test exp value ?(env = Oops.Env.create [] None) = fun _ -> assert_equal (Oops.Eval.eval exp env) value
let eval_tests = "eval tests" >::: [
  "empty block" >:: make_eval_test (Block []) Null;
  "null" >:: make_eval_test Null Null;
  "bool" >:: make_eval_test (Bool true) (Bool true);
  "number" >:: make_eval_test (Number (-5.67)) (Number (-5.67));
  "string" >:: make_eval_test (String "goodbye cruel world") (String "goodbye cruel world");
  "list" >:: make_eval_test (List [Null; Number 1.0; String "a"]) (List [Null; Number 1.0; String "a"]);
  "dict" >:: make_eval_test (Dict [(Number 1.0, String "a"); (String "b", Number 2.0)]) (Dict ([(Value.Number 1.0, Value.String "a"); (String "b", Number 2.0)] |> List.to_seq |> Hashtbl.of_seq));
  "fun" >:: make_eval_test (Fun (["a"; "b"], Block [Var "a"; Var "b"])) (Function ("", ["a"; "b"], Block [Var "a"; Var "b"], Env.create [] None));
  "var" >:: make_eval_test (Var "x") (Number 2.3) ~env: (Env.create [("x", Value.Number 2.3)] None);
  "dot" >:: make_eval_test (Dot (Var "x", "a")) (Number 5.0) ~env: (Env.create [("x", Value.Struct (Oops.Value.make_type "S" ["a"; "b"], [("a", Value.Number 5.0); ("b", String "")] |> List.to_seq |> Hashtbl.of_seq))] None);
  "call" >:: make_eval_test (Call (Var "f", [Number 5.0])) (Number 6.0) ~env: (Env.create [("f", Value.Primitive (fun [Number x] -> Number (x +. 1.0)))] None);
  "not" >:: make_eval_test (Call (Dot (Bool true, "not"), [])) (Bool false);
  "negate" >:: make_eval_test (Call (Dot (Number 6.5, "u-"), [])) (Number (-6.5));
  "or" >:: make_eval_test (Call (Dot (Bool false, "or"), [Bool true])) (Bool true);
  "and" >:: make_eval_test (Call (Dot (Bool false, "and"), [Bool true])) (Bool false);
  "<" >:: make_eval_test (Call (Dot (Number 0.0, "<"), [Number 0.1])) (Bool true);
  "<=" >:: make_eval_test (Call (Dot (Number 0.0, "<="), [Number 0.1])) (Bool true);
  "==" >:: make_eval_test (Call (Dot (Number 0.0, "=="), [Number 0.1])) (Bool false);
  "!=" >:: make_eval_test (Call (Dot (Number 0.0, "!="), [Number 0.1])) (Bool true);
  ">" >:: make_eval_test (Call (Dot (Number 0.0, ">"), [Number 0.1])) (Bool false);
  ">=" >:: make_eval_test (Call (Dot (Number 0.0, ">="), [Number 0.1])) (Bool false);
  "::" >:: make_eval_test (Call (Dot (Call (Dot (Call (Dot (List [], "::"), [Number 3.0]), "::"), [Number 2.0]), "::"), [Number 1.0])) (List [Number 1.0; Number 2.0; Number 3.0]);
  "+" >:: make_eval_test (Call (Dot (Number 1.0, "+"), [Number 3.0])) (Number 4.0);
  "-" >:: make_eval_test (Call (Dot (Number 1.0, "-"), [Number 3.0])) (Number (-2.0));
  "*" >:: make_eval_test (Call (Dot (Number 1.0, "*"), [Number 3.0])) (Number (3.0));
  "/" >:: make_eval_test (Call (Dot (Number 1.0, "/"), [Number 4.0])) (Number 0.25);
  "%" >:: make_eval_test (Call (Dot (Number 8.0, "%"), [Number 3.0])) (Number 2.0);
  "if" >:: make_eval_test (If [(Bool false, Number 1.0); (Bool true, Number 2.0); (Bool true, Number 3.0)]) (Number 2.0);
  "match literal" >:: make_eval_test (Match (Number 1.0, [(Number 2.0, String "a"); (Number 1.0, String "b")])) (String "b");
  "match" >:: make_eval_test (Match (List [Number 1.0; Number 2.0], [(Call (Dot (Var "y", "::"), [Var "x"]), Var "y")])) (List [Number 2.0]);
  "let" >:: make_eval_test (Let ([Var "x", Number 3.5], Var "x")) (Number 3.5);
  "assign" >:: make_eval_test (Block [Assign (Var "x", Number 5.0); Var "x"]) (Number 5.0);
  "dot assign" >:: make_eval_test (Block [DotAssign (Var "x", "a", Number 5.0); Dot (Var "x", "a")]) (Number 5.0) ~env: (Env.create [("x", Value.Struct (Oops.Value.make_type "S" ["a"; "b"], [("a", Value.Number 5.0); ("b", String "")] |> List.to_seq |> Hashtbl.of_seq))] None);
  "def" >:: make_eval_test (Block [Def ("f", ["x"], Number 4.5); Call (Var "f", [Number 0.0])]) (Number 4.5);
  "struct" >:: make_eval_test (Block [Struct ("S", ["a"; "b"]); Dot (Call (Var "S", [Number 1.0; String "a"]), "b")]) (String "a");
  "impl" >:: make_eval_test (Block [Trait ("T", ["f"; "g"], [("h", [], Bool false)]); Struct ("S", ["a"; "b"]); Impl (Some (Var "T"), Var "S", [("f", [], Null); ("g", [], Null)]); Call (Dot (Call (Var "S", [Null; Null]), "h"), [])]) (Bool false);
]

let _ =
  run_test_tt_main parse_tests;
  run_test_tt_main eval_tests