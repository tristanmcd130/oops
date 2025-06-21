open OUnit2
open Oops

let make_parser_test string tree _ = assert_equal tree (string |> Lexing.from_string |> Parser.program Lexer.read) ~printer: Ast.to_string
let make_parser_error_test string error _ = assert_raises error (fun _ -> string |> Lexing.from_string |> Parser.program Lexer.read)
let parser_tests = "parser tests" >::: [
  "empty" >:: make_parser_test "" (Block []);
  "block" >:: make_parser_test "3.4e-5 5 -6" (Block [Number 3.4e-5; Number 5.0; Number (-6.0)]);
  "null" >:: make_parser_test "null" Null;
  "bool" >:: make_parser_test "true false" (Block [Bool true; Bool false]);
  "number" >:: make_parser_test "3.4e-5" (Number 3.4e-5);
  "string" >:: make_parser_test "\"a\"" (String "a");
  "escaped string" >:: make_parser_test "\"\\tI said, \\\"Hello world!\\\"\\n\"" (String "\tI said, \"Hello world!\"\n");
  "unterminated string" >:: make_parser_error_test "\"aaaaaaa" (Failure "Unterminated string");
  "list" >:: make_parser_test "[1, null, \"b\"]" (List [Number 1.0; Null; String "b"]);
  "map" >:: make_parser_test "{1: false, null: [], [3]: \"b\"}" (Map [(Number 1.0, Bool false); (Null, List []); (List [Number 3.0], String "b")]);
  "unexpected char" >:: make_parser_error_test "@" (Failure "Unexpected character: @");
  "var" >:: make_parser_test "t" (Var "t");
  "assign" >:: make_parser_test "t = 2" (Assign ("t", Number 2.0));
  "add" >:: make_parser_test "4 + 5" (Binary (Number 4.0, Add, Number 5.0));
  "subtract" >:: make_parser_test "4 - 5" (Binary (Number 4.0, Subtract, Number 5.0));
  "multiply" >:: make_parser_test "4 * 5" (Binary (Number 4.0, Multiply, Number 5.0));
  "divide" >:: make_parser_test "4 / 5" (Binary (Number 4.0, Divide, Number 5.0));
  "modulo" >:: make_parser_test "4 % 5" (Binary (Number 4.0, Modulo, Number 5.0));
  "fun" >:: make_parser_test "fun(x, y) x end" (Fun (["x"; "y"], Var "x"));
  "def" >:: make_parser_test "def f(x) x 4 end" (Assign ("f", Fun (["x"], Block [Var "x"; Number 4.0])));
  "call" >:: make_parser_test "5(7)" (Call (Number 5.0, [Number 7.0]));
]

let make_compiler_test tree chunk _ =
  let c = Chunk.empty () in
  Chunk.compile c (Scope.make None) tree;
  assert_equal chunk c ~printer: Chunk.to_string
let compiler_tests = "compiler tests" >::: [
  "empty" >:: make_compiler_test (Ast.Block []) (Chunk.empty ());
  "block" >:: make_compiler_test (Block [Number 3.4e-5; Number 5.0; Number (-6.0)]) (Chunk.make [|GetConstant 0; GetConstant 1; GetConstant 2|] [|Number 3.4e-5; Number 5.0; Number (-6.0)|] [||]);
  "null" >:: make_compiler_test Null (Chunk.make [|GetConstant 0|] [|Null|] [||]);
  "bool" >:: make_compiler_test (Block [Bool true; Bool false]) (Chunk.make [|GetConstant 0; GetConstant 1|] [|Bool true; Bool false|] [||]);
  "number" >:: make_compiler_test (Number 3.4e-5) (Chunk.make [|GetConstant 0|] [|Number 3.4e-5|] [||]);
  "string" >:: make_compiler_test (String "a") (Chunk.make [|GetConstant 0|] [|String "a"|] [||]);
  "list" >:: make_compiler_test (List [Number 1.0; Null; String "b"]) (Chunk.make [|GetConstant 0; GetConstant 1; GetConstant 2; MakeList 3|] [|Number 1.0; Null; String "b"|] [||]);
  "map" >:: make_compiler_test (Map [(Number 1.0, Bool false); (Null, List []); (List [Number 3.0], String "b")]) (Chunk.make [|GetConstant 0; GetConstant 1; GetConstant 2; MakeList 0; GetConstant 3; MakeList 1; GetConstant 4; MakeMap 3|] [|Number 1.0; Bool false; Null; Number 3.0; String "b"|] [||]);
  "repeated constants" >:: make_compiler_test (Block [Number 5.0; Number 5.0]) (Chunk.make [|GetConstant 0; GetConstant 0|] [|Number 5.0|] [||]);
  "var" >:: make_compiler_test (Var "t") (Chunk.make [|GetGlobal 0|] [||] [|"t"|]);
  "assign" >:: make_compiler_test (Assign ("t", Number 2.0)) (Chunk.make [|GetConstant 0; SetGlobal 0|] [|Number 2.0|] [|"t"|]);
  "add" >:: make_compiler_test (Binary (Number 4.0, Add, Number 5.0)) (Chunk.make [|GetConstant 0; GetConstant 1; Add|] [|Number 4.0; Number 5.0|] [||]);
  "subtract" >:: make_compiler_test (Binary (Number 4.0, Subtract, Number 5.0)) (Chunk.make [|GetConstant 0; GetConstant 1; Subtract|] [|Number 4.0; Number 5.0|] [||]);
  "multiply" >:: make_compiler_test (Binary (Number 4.0, Multiply, Number 5.0)) (Chunk.make [|GetConstant 0; GetConstant 1; Multiply|] [|Number 4.0; Number 5.0|] [||]);
  "divide" >:: make_compiler_test (Binary (Number 4.0, Divide, Number 5.0)) (Chunk.make [|GetConstant 0; GetConstant 1; Divide|] [|Number 4.0; Number 5.0|] [||]);
  "modulo" >:: make_compiler_test (Binary (Number 4.0, Modulo, Number 5.0)) (Chunk.make [|GetConstant 0; GetConstant 1; Modulo|] [|Number 4.0; Number 5.0|] [||]);
  "fun" >:: make_compiler_test (Fun (["x"], Var "x")) (Chunk.make [|GetConstant 0; MakeClosure 0|] [|Function (Value.make_function (Chunk.make [|GetLocal 0|] [||] [||]) 1 0)|] [||]);
  "def" >:: make_compiler_test (Assign ("f", Fun (["x"], Block [Var "x"; Number 4.0]))) (Chunk.make [|GetConstant 0; MakeClosure 0; SetGlobal 0|] [|Function (Value.make_function (Chunk.make [|GetLocal 0; GetConstant 0|] [|Number 4.0|] [||]) 1 0)|] [|"f"|]);
  "call" >:: make_compiler_test (Call (Fun (["x"], Binary (Var "x", Add, Number 4.0)), [Number 9.0])) (Chunk.make [|GetConstant 0; GetConstant 1; MakeClosure 0; Call 1|] [|Number 9.0; Function (Value.make_function (Chunk.make [|GetLocal 0; GetConstant 0; Add|] [|Number 4.0|] [||]) 1 0)|] [||]);
  "closure" >:: make_compiler_test (Fun ([], Block [Assign ("x", Number 1.0); Fun ([], Var "x")])) (Chunk.make [|GetConstant 0; MakeClosure 0|] [|Function (Value.make_function (Chunk.make [|GetConstant 0; SetLocal 0; MakeCell 0; GetConstant 1; MakeClosure 1|] [|Number 1.0; Function (Value.make_function (Chunk.make [|DerefUpvalue 0|] [||] [||]) 0 0)|] [||]) 0 1)|] [||]);
]

let make_vm_test ?(globals = []) closure result _ = assert_equal result (Vm.call (globals |> List.to_seq |> Hashtbl.of_seq |> Vm.make) closure []) ~printer: Value.to_string
let make_vm_error_test ?(globals = []) closure error _ = assert_raises error (fun _ -> Vm.call (globals |> List.to_seq |> Hashtbl.of_seq |> Vm.make) closure [])
let vm_tests = "vm tests" >::: [
  "empty" >:: make_vm_test (Chunk.empty () |> Chunk.to_closure) Null;
  "block" >:: make_vm_test (Chunk.make [|GetConstant 0; GetConstant 1; GetConstant 2|] [|Number 3.4e-5; String "p"; Number (-6.0)|] [||] |> Chunk.to_closure) (Number (-6.0));
  "get_constant" >:: make_vm_test (Chunk.make [|GetConstant 0|] [|Number 3.4e-5|] [||] |> Chunk.to_closure) (Number 3.4e-5);
  "make_list" >:: make_vm_test (Chunk.make [|GetConstant 0; GetConstant 1; GetConstant 2; MakeList 3|] [|Number 1.0; Null; String "b"|] [||] |> Chunk.to_closure) (List [Number 1.0; Null; String "b"]);
  "make_map" >:: make_vm_test (Chunk.make [|GetConstant 0; GetConstant 1; GetConstant 2; GetConstant 3; MakeList 0; GetConstant 3; MakeMap 3|] [|Number 1.0; String "a"; Null; Bool true|] [||] |> Chunk.to_closure) (Map ([(Value.Number 1.0, Value.String "a"); (Null, Bool true); (List [], Bool true)] |> List.to_seq |> Hashtbl.of_seq));
  "get_global" >:: make_vm_test ~globals: [("t", Number 2.0)] (Chunk.make [|GetGlobal 0|] [||] [|"t"|] |> Chunk.to_closure) (Number 2.0);
  "get_global undefined" >:: make_vm_error_test (Chunk.make [|GetGlobal 0|] [||] [|"t"|] |> Chunk.to_closure) (Failure ("Undefined global variable: t"));
  "set_global" >:: make_vm_test (Chunk.make [|GetConstant 0; SetGlobal 0; GetGlobal 0|] [|Number 2.0|] [|"t"|] |> Chunk.to_closure) (Number 2.0);
  "set_global returns null" >:: make_vm_test (Chunk.make [|GetConstant 0; SetGlobal 0|] [|Number 2.0|] [|"t"|] |> Chunk.to_closure) Null;
  "add" >:: make_vm_test (Chunk.make [|GetConstant 0; GetConstant 1; Add|] [|Number 4.0; Number 5.0|] [||] |> Chunk.to_closure) (Number 9.0);
  "add strings" >:: make_vm_test (Chunk.make [|GetConstant 0; GetConstant 1; Add|] [|String "4"; String "5"|] [||] |> Chunk.to_closure) (String "45");
  "subtract" >:: make_vm_test (Chunk.make [|GetConstant 0; GetConstant 1; Subtract|] [|Number 4.0; Number 5.0|] [||] |> Chunk.to_closure) (Number (-1.0));
  "multiply" >:: make_vm_test (Chunk.make [|GetConstant 0; GetConstant 1; Multiply|] [|Number 4.0; Number 5.0|] [||] |> Chunk.to_closure) (Number 20.0);
  "divide" >:: make_vm_test (Chunk.make [|GetConstant 0; GetConstant 1; Divide|] [|Number 4.0; Number 5.0|] [||] |> Chunk.to_closure) (Number 0.8);
  "modulo" >:: make_vm_test (Chunk.make [|GetConstant 0; GetConstant 1; Modulo|] [|Number 4.0; Number 5.0|] [||] |> Chunk.to_closure) (Number 4.0);
  "get_local" >:: make_vm_test (Chunk.make [|GetConstant 0|] [|Function (Value.make_function (Chunk.make [|GetLocal 0|] [||] [||]) 1 0)|] [||] |> Chunk.to_closure) (Function (Value.make_function (Chunk.make [|GetLocal 0|] [||] [||]) 1 0));
  "set_local" >:: make_vm_test (Value.make_closure (Value.make_function (Chunk.make [|GetConstant 0; SetLocal 0; GetLocal 0|] [|String "c"|] [||]) 0 1) [||]) (String "c");
  "call" >:: make_vm_test (Chunk.make [|GetConstant 0; GetConstant 1; MakeClosure 0; Call 1|] [|Number 9.0; Function (Value.make_function (Chunk.make [|GetLocal 0; GetConstant 0; Add|] [|Number 4.0|] [||]) 1 0)|] [||] |> Chunk.to_closure) (Number 13.0);
  "call with bad args" >:: make_vm_error_test (Chunk.make [|GetConstant 0; MakeClosure 0; Call 0|] [|Function (Value.make_function (Chunk.make [|GetLocal 0; GetConstant 0; Add|] [|Number 4.0|] [||]) 1 0)|] [||] |> Chunk.to_closure) (Failure "Function expected 1 arguments, but received 0");
  "call with bad func" >:: make_vm_error_test (Chunk.make [|GetConstant 0; Call 0|] [|Number 5.0|] [||] |> Chunk.to_closure) (Failure "Cannot call 5");
  "deref_upvalue" >:: make_vm_test (Value.make_closure (Value.make_function (Chunk.make [|DerefUpvalue 0|] [||] [||]) 0 0) [|ref (Value.Number 15.0)|]) (Number 15.0);
  "closure" >:: make_vm_test (Chunk.make [|GetConstant 0; MakeClosure 0; Call 0; Call 0|] [|Function (Value.make_function (Chunk.make [|GetConstant 0; SetLocal 0; MakeCell 0; GetConstant 1; MakeClosure 1|] [|Number 1.0; Function (Value.make_function (Chunk.make [|DerefUpvalue 0|] [||] [||]) 0 0)|] [||]) 0 1)|] [||] |> Chunk.to_closure) (Number 1.0);
]

let _ =
  run_test_tt_main parser_tests;
  run_test_tt_main compiler_tests;
  run_test_tt_main vm_tests