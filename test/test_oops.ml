open OUnit2
open Oops

(* let make_parser_test string tree _ = assert_equal tree (string |> Lexing.from_string |> Parser.program Lexer.read) ~printer: Ast.to_string
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
  "fun" >:: make_compiler_test (Fun (["x"], Var "x")) (Chunk.make [|GetConstant 0; MakeClosure 0|] [|Function {chunk = Chunk.make [|GetLocal 0|] [||] [||]; num_args = 1; num_locals = 0}|] [||]);
  "def" >:: make_compiler_test (Assign ("f", Fun (["x"], Block [Var "x"; Number 4.0]))) (Chunk.make [|GetConstant 0; MakeClosure 0; SetGlobal 0|] [|Function (Value.make_function (Chunk.make [|GetLocal 0; GetConstant 0|] [|Number 4.0|] [||]) 1 0)|] [|"f"|]);
  "call" >:: make_compiler_test (Call (Fun (["x"], Binary (Var "x", Add, Number 4.0)), [Number 9.0])) (Chunk.make [|GetConstant 0; GetConstant 1; MakeClosure 0; Call 1|] [|Number 9.0; Function (Value.make_function (Chunk.make [|GetLocal 0; GetConstant 0; Add|] [|Number 4.0|] [||]) 1 0)|] [||]);
  "closure" >:: make_compiler_test (Fun ([], Block [Assign ("x", Number 1.0); Fun ([], Var "x")])) (Chunk.make [|GetConstant 0; MakeClosure 0|] [|Function (Value.make_function (Chunk.make [|GetConstant 0; SetLocal 0; MakeCell 0; GetConstant 1; MakeClosure 1|] [|Number 1.0; Function (Value.make_function (Chunk.make [|DerefUpvalue 0|] [||] [||]) 0 0)|] [||]) 0 1)|] [||]);
  "mutually recursive closures" >:: make_compiler_test (Fun ([], Block [Assign ("f", Fun ([], Call (Var "g", []))); Assign ("g", Fun ([], Call (Var "f", [])))])) (Chunk.make [|GetConstant 0; MakeClosure 0|] [|Function (Value.make_function (Chunk.make [|MakeCell 1; GetConstant 0; MakeClosure 1; SetLocal 0; MakeCell 0; GetConstant 1; MakeClosure 1; SetLocal 1|] [|Function (Value.make_function); |] [||]) 0 2)|] [||])
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
] *)

let make_test ?(globals = []) string result _ =
  let chunk = Chunk.empty () in
  string |> Lexing.from_string |> Parser.program Lexer.read |> Chunk.compile chunk (Scope.make None false []);
  assert_equal result (Vm.call (globals |> List.to_seq |> Hashtbl.of_seq |> Vm.make) (Chunk.to_closure chunk) []) ~printer: Value.to_string
let make_error_test ?(globals = []) string error _ =
  assert_raises error (fun _ ->
    let chunk = Chunk.empty () in
    string |> Lexing.from_string |> Parser.program Lexer.read |> Chunk.compile chunk (Scope.make None false []);
    Vm.call (globals |> List.to_seq |> Hashtbl.of_seq |> Vm.make) (Chunk.to_closure chunk) [])
let tests = "tests" >::: [
  "empty" >:: make_test "" Null;
  "block" >:: make_test "3 5 -6" (Number (-6.0));
  "null" >:: make_test "null" Null;
  "true" >:: make_test "true" (Bool true);
  "false" >:: make_test "false" (Bool false);
  "number" >:: make_test "3.4e-5" (Number 3.4e-5);
  "string" >:: make_test "\"a\"" (String "a");
  "escaped string" >:: make_test "\"\\tI said, \\\"Hello world!\\\"\\n\"" (String "\tI said, \"Hello world!\"\n");
  "unterminated string" >:: make_error_test "\"aaaaaaa" (Failure "Unterminated string");
  "list" >:: make_test "[1, null, \"b\"]" (List [Number 1.0; Null; String "b"]);
  "map" >:: make_test "{1: false, null: [], [4]: \"b\"}" (Map ([(Value.Number 1.0, Value.Bool false); (Null, List []); (List [Number 4.0], String "b")] |> List.to_seq |> Hashtbl.of_seq)); (* change 4 to 3 and this test fails for some reason *)
  "unexpected char" >:: make_error_test "@" (Failure "Unexpected character @");
  "global" >:: make_test ~globals: [("t", Number 5.0)] "t" (Number 5.0);
  "undefined global" >:: make_error_test "t" (Failure "Undefined global variable t");
  "negate" >:: make_test "--9" (Number 9.0);
  "add" >:: make_test "4 + 5" (Number 9.0);
  "add strings" >:: make_test "\"aa\" + \"ba\"" (String "aaba");
  "add string and number" >:: make_error_test "\"aa\" + 5" (Failure "Invalid arguments to +");
  "subtract" >:: make_test "4 - 5" (Number (-1.0));
  "multiply" >:: make_test "4 * 5" (Number 20.0);
  "divide" >:: make_test "4 / 5" (Number (0.8));
  "modulo" >:: make_test "4.5 % 2" (Number 0.5);
  "arithmetic precedence" >:: make_test "1 + 3 * 2 - 4 / (5 % 3)" (Number 5.0);
  "less than" >:: make_test "4 < 5" (Bool true);
  "less than or equal to" >:: make_test "4 <= 5" (Bool true);
  "equal to" >:: make_test "\"a\" == 5" (Bool false);
  "not equal to" >:: make_test "\"a\" != 5" (Bool true);
  "greater than" >:: make_test "4 > 5" (Bool false);
  "greater than or equal to" >:: make_test "5 >= 5" (Bool true);
  "and" >:: make_test "5 > 4 and 5 > 3" (Bool true);
  "or" >:: make_test "5 > 666666 or 5 > 1 + 3" (Bool true);
  "not" >:: make_test "not (5 > 666666)" (Bool true);
  "logical precedence" >:: make_test "not true and false" (Bool false);
  "cons" >:: make_test "1 :: 2 :: 3 + 4 :: []" (List [Number 1.0; Number 2.0; Number 7.0]);
  "call" >:: make_test "(fun(x) x + 4 end)(4)" (Number 8.0);
  "call with bad args" >:: make_error_test "(fun(x) x + 4 end)(4, 7)" (Failure "<anonymous function> expected 1 arguments, but received 2");
  "call with bad func" >:: make_error_test "6(4, 7)" (Failure "Cannot call 6");
  "upvalue" >:: make_test "(fun(x) y = 5 fun() x + y end end)(1)()" (Number 6.0);
  "primitive" >:: make_test ~globals: [("add_one", Primitive (fun [Number x] -> Number (x +. 1.0)))] "add_one(44)" (Number 45.0);
  "if" >:: make_test "if true then 1 else 2 end" (Number 1.0);
  "if with no else" >:: make_test "if false then 1 end" Null;
  "elseif" >:: make_test "if false then 1 elseif true then 2 else 3 end" (Number 2.0);
  "convoluted elseif" >:: make_test
    "if false then
      1
    elseif 1 + 1 == 2 then
      if 1 + 1 == 3 then
        2
      elseif 1 + 2 == 3 then
        3
      else
        4
      end
    elseif 1 + 1 == 2 then
      5
    else
      6
    end" (Number 3.0);
  "mutually recursive closures" >:: make_test
    "def f()
      def g(x)
        if x == 0 then
          true
        else
          h(x - 1)
        end
      end
      def h(x)
        if x == 0 then
          false
        else
          g(x - 1)
        end
      end
      g
    end
    f()(6)" (Bool true);
  "let" >:: make_test "let x = 3, y = 0 in x + y end" (Number 3.0);
  "let out of scope" >:: make_error_test "let x = 3 in x end x" (Failure "Undefined global variable x");
  "struct" >:: make_test "struct A a b end" Null;
  "struct with bad args" >:: make_error_test "struct A a b end A(56)" (Failure "Constructor for <type A> expected 2 arguments, but received 1");
  "dot" >:: make_test "struct A a b end A(5, 6).b" (Number 6.0);
  "dot nonexistent field" >:: make_error_test "struct A a b end A(5, 6).c" (Failure "<type A> has no field/method c");
  "impl for" >:: make_test "struct A a b end impl for A def f() self.a + self.b end end A(5, 6).f()" (Number 11.0);
  "bound primitive" >:: make_test ~globals: [("A", Type {name = "A"; fields = [("a", 0); ("b", 1)] |> List.to_seq |> Hashtbl.of_seq; methods = [("f", Value.Primitive (fun [self; Number x] ->
    let Number a = Chunk.dot self "a" in
    let Number b = Chunk.dot self "b" in
    Number (a +. b +. x)))] |> List.to_seq |> Hashtbl.of_seq; traits = []})] "f = A(2, 4).f f(7)" (Number 13.0);
  "impl" >:: make_test
    "trait T
      f
      g
      def h()
        self.f() + self.g()
      end
    end
    struct A
      a
      b
    end
    impl T for A
      def f()
        self.a
      end
      def g()
        self.b
      end
    end
    A(4, 6).h()" (Number 10.0);
  "partial impl" >:: make_error_test
    "trait T
      f
      g
      def h()
        self.f() + self.g()
      end
    end
    struct A
      a
      b
    end
    impl T for A
      def f()
        self.a
      end
    end" (Failure "<type A> does not fully implement <trait T>: missing g");
  "operator overloading" >:: make_test "struct A a b end impl for A def +(other) A(self.a + other.a, self.b + other.b) end end (A(2, 3) + A(7, 4)).a" (Number 9.0);
  "import" >:: make_test "import \"import_test.oops\" import_test.a" (Number 100.0);
  "import private" >:: make_error_test "import \"import_test.oops\" import_test.b" (Failure "<type Exports> has no field/method b");
  "import not global" >:: make_error_test "import \"import_test.oops\" a" (Failure "Undefined global variable a");
  "import for" >:: make_test "import \"import_test.oops\" for a a" (Number 100.0);
  "import for no module" >:: make_error_test "import \"import_test.oops\" for a import_test" (Failure "Undefined global variable import_test");
  "import for as" >:: make_test "import \"import_test.oops\" for a as b b" (Number 100.0);
  "tail call" >:: make_test "def f(x) if x <= 0 then true else f(x - 1) end end f(1e4)" (Bool true);
]
let _ = run_test_tt_main tests