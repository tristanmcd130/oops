open OUnit2
open Oops

let make_test ?(module_vars = []) string result _ =
  let chunk = Chunk.empty () in
  let module' = Module.make "" module_vars in
  string |> Lexing.from_string |> Parser.program Lexer.read |> Chunk.compile chunk (Scope.make None []) module';
  assert_equal result (Vm.call (Vm.make ()) (Chunk.to_closure chunk module') []) ~printer: Value.to_string
let make_error_test ?(module_vars = []) string error _ =
  assert_raises error (fun _ ->
    let chunk = Chunk.empty () in
    let module' = Module.make "" module_vars in
    string |> Lexing.from_string |> Parser.program Lexer.read |> Chunk.compile chunk (Scope.make None []) module';
    Vm.call (Vm.make ()) (Chunk.to_closure chunk module') [])
let tests = "tests" >::: [
  "empty" >:: make_test "" (List []);
  "block" >:: make_test "3 5 -6" (Number (-6.0));
  "true" >:: make_test "true" (Bool true);
  "false" >:: make_test "false" (Bool false);
  "number" >:: make_test "3.4e-5" (Number 3.4e-5);
  "string" >:: make_test "\"a\"" (String "a");
  "escaped string" >:: make_test "\"\\tI said, \\\"Hello world!\\\"\\n\"" (String "\tI said, \"Hello world!\"\n");
  "unterminated string" >:: make_error_test "\"aaaaaaa" (Failure "Unterminated string");
  "list" >:: make_test "[1, [], \"b\"]" (List [Number 1.0; List []; String "b"]);
  "map" >:: make_test "{1: false, true: [], [4]: \"b\"}" (Map ([(Types.Number 1.0, Types.Bool false); (Bool true, List []); (List [Number 4.0], String "b")] |> List.to_seq |> Hashtbl.of_seq)); (* change 4 to 3 and this test fails for some reason *)
  "unexpected char" >:: make_error_test "@" (Failure "Unexpected character @");
  "global" >:: make_test ~module_vars: [("t", Number 5.0)] "t" (Number 5.0);
  "undefined global" >:: make_error_test "t" (Failure "Undefined global variable t");
  "negate" >:: make_test "--9" (Number 9.0);
  "add" >:: make_test "4 + 5" (Number 9.0);
  "add strings" >:: make_test "\"aa\" + \"ba\"" (String "aaba");
  "add string and number" >:: make_error_test "\"aa\" + 5" (Failure "Invalid arguments to primitive function");
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
  "primitive" >:: make_test ~module_vars: [("add_one", Primitive (fun [Number x] -> Number (x +. 1.0)))] "add_one(44)" (Number 45.0);
  "if" >:: make_test "if true then 1 else 2 end" (Number 1.0);
  "if with no else" >:: make_test "if false then 1 end" (List []);
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
  "struct" >:: make_test "struct A a b end" (List []);
  "struct with bad args" >:: make_error_test "struct A a b end A(56)" (Failure "Constructor for <type A> expected 2 arguments, but received 1");
  "dot" >:: make_test "struct A a b end A(5, 6).b" (Number 6.0);
  "dot nonexistent field" >:: make_error_test "struct A a b end A(5, 6).c" (Failure "<type A> has no field/method c");
  "dot assign" >:: make_test ~module_vars: [("A", Type {name = "A"; fields = [("a", 0); ("b", 1)] |> List.to_seq |> Hashtbl.of_seq; methods = Hashtbl.create 0; traits = []})] "a = A(5, 6) a.b = 8.5 [a.a, a.b]" (List [Number 5.0; Number 8.5]);
  "dot assign nonexistent field" >:: make_error_test ~module_vars: [("A", Type {name = "A"; fields = [("a", 0); ("b", 1)] |> List.to_seq |> Hashtbl.of_seq; methods = Hashtbl.create 0; traits = []})] "a = A(5, 6) a.c = 8.5" (Failure "<type A> has no field c");
  "impl for" >:: make_test "struct A a b end impl for A def f() self.a + self.b end end A(5, 6).f()" (Number 11.0);
  "bound primitive" >:: make_test ~module_vars: [("A", Type {name = "A"; fields = [("a", 0); ("b", 1)] |> List.to_seq |> Hashtbl.of_seq; methods = [("f", Types.Primitive (fun [self; Number x] ->
    let Number a = Value.get_field self "a" in
    let Number b = Value.get_field self "b" in
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
  "import private" >:: make_error_test "import \"import_test.oops\" import_test.b" (Failure "<module from import_test.oops> does not export b");
  "import not global" >:: make_error_test "import \"import_test.oops\" a" (Failure "Undefined global variable a");
  "import for" >:: make_test "import \"import_test.oops\" for a a" (Number 100.0);
  "import for no module" >:: make_error_test "import \"import_test.oops\" for a import_test" (Failure "Undefined global variable import_test");
  "import for as" >:: make_test "import \"import_test.oops\" for a as b b" (Number 100.0);
  "import as" >:: make_test "import \"import_test.oops\" as m m.a" (Number 100.0);
  "tail call" >:: make_test "def f(x) if x <= 0 then true else f(x - 1) end end f(1e4)" (Bool true);
  "throw" >:: make_test "throw 6" (List []);
  "try" >:: make_test "try throw 8 catch n n end" (Number 8.0);
  "empty try" >:: make_test "fun(x, y) x == [] end(try catch n n end, 5)" (Bool true);
  "try in catch" >:: make_test
  "try
    throw 5
  catch n
    try
      throw n + 1
    catch m
      m + 1
    end
  end" (Number 7.0);
  "try no error" >:: make_test "try 4 catch n n + 2 end" (Number 4.0);
  "match literal" >:: make_test "match 4 case 4 then \"b\" end" (String "b");
]
let _ = run_test_tt_main tests