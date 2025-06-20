open OUnit2
open Oops

let make_parser_test string tree _ = assert_equal tree (string |> Lexing.from_string |> Parser.program Lexer.read) ~printer: Ast.to_string
let parser_tests = "parser tests" >::: [
  "empty" >:: make_parser_test "" (Block []);
  "block" >:: make_parser_test "3.4e-5 5 -6" (Block [Number 3.4e-5; Number 5.0; Number (-6.0)]);
  "null" >:: make_parser_test "null" Null;
  "bool" >:: make_parser_test "true false" (Block [Bool true; Bool false]);
  "number" >:: make_parser_test "3.4e-5" (Number 3.4e-5);
  "string" >:: make_parser_test "\"a\"" (String "a");
  "escaped string" >:: make_parser_test "\"\\tI said, \\\"Hello world!\\\"\\n\"" (String "\tI said, \"Hello world!\"\n");
  "list" >:: make_parser_test "[1, null, \"b\"]" (List [Number 1.0; Null; String "b"]);
  "map" >:: make_parser_test "{1: false, null: [], [3]: \"b\"}" (Map [(Number 1.0, Bool false); (Null, List []); (List [Number 3.0], String "b")]);
]

let make_compiler_test tree chunk _ =
  let c: Chunk.t = {code = [||]; constants = [||]; names = [||]} in
  Chunk.compile c tree;
  assert_equal chunk c ~printer: Chunk.to_string
let compiler_tests = "compiler tests" >::: [
  "empty" >:: make_compiler_test (Block []) {code = [||]; constants = [||]; names = [||]};
  "block" >:: make_compiler_test (Block [Number 3.4e-5; Number 5.0; Number (-6.0)]) {code = [|GetConstant 0; GetConstant 1; GetConstant 2|]; constants = [|Number 3.4e-5; Number 5.0; Number (-6.0)|]; names = [||]};
  "null" >:: make_compiler_test Null {code = [|GetConstant 0|]; constants = [|Null|]; names = [||]};
  "bool" >:: make_compiler_test (Block [Bool true; Bool false]) {code = [|GetConstant 0; GetConstant 1|]; constants = [|Bool true; Bool false|]; names = [||]};
  "number" >:: make_compiler_test (Number 3.4e-5) {code = [|GetConstant 0|]; constants = [|Number 3.4e-5|]; names = [||]};
  "string" >:: make_compiler_test (String "a") {code = [|GetConstant 0|]; constants = [|String "a"|]; names = [||]};
  "list" >:: make_compiler_test (List [Number 1.0; Null; String "b"]) {code = [|GetConstant 0; GetConstant 1; GetConstant 2; MakeList 3|]; constants = [|Number 1.0; Null; String "b"|]; names = [||]};
  "map" >:: make_compiler_test (Map [(Number 1.0, Bool false); (Null, List []); (List [Number 3.0], String "b")]) {code = [|GetConstant 0; GetConstant 1; GetConstant 2; MakeList 0; GetConstant 3; MakeList 1; GetConstant 4; MakeMap 3|]; constants = [|Number 1.0; Bool false; Null; Number 3.0; String "b"|]; names = [||]};
  "repeated constants" >:: make_compiler_test (Block [Number 5.0; Number 5.0]) {code = [|GetConstant 0; GetConstant 0|]; constants = [|Number 5.0|]; names = [||]};
]

let make_vm_test chunk result _ = assert_equal result (Vm.call (Hashtbl.create 16 |> Vm.make) {func = {chunk; num_args = 0; num_locals = 0}; upvalues = [||]} []) ~printer: Value.to_string
let vm_tests = "vm tests" >::: [
  "empty" >:: make_vm_test {code = [||]; constants = [||]; names = [||]} Null;
  "block" >:: make_vm_test {code = [|GetConstant 0; GetConstant 1; GetConstant 2|]; constants = [|Number 3.4e-5; String "p"; Number (-6.0)|]; names = [||]} (Number (-6.0));
  "get_constant" >:: make_vm_test {code = [|GetConstant 0|]; constants = [|Number 3.4e-5|]; names = [||]} (Number 3.4e-5);
  "make_list" >:: make_vm_test {code = [|GetConstant 0; GetConstant 1; GetConstant 2; MakeList 3|]; constants = [|Number 1.0; Null; String "b"|]; names = [||]} (List [Number 1.0; Null; String "b"]);
  "make_map" >:: make_vm_test {code = [|GetConstant 0; GetConstant 1; GetConstant 2; GetConstant 3; MakeList 0; GetConstant 3; MakeMap 3|]; constants = [|Number 1.0; String "a"; Null; Bool true|]; names = [||]} (Map ([(Value.Number 1.0, Value.String "a"); (Null, Bool true); (List [], Bool true)] |> List.to_seq |> Hashtbl.of_seq));
]

let _ =
  run_test_tt_main parser_tests;
  run_test_tt_main compiler_tests;
  run_test_tt_main vm_tests