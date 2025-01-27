open OUnit2
open Oops

let eval_tests = "eval tests" >::: [
  "block" >:: (fun _ -> assert_equal 1 1)
]

let _ = run_test_tt_main tests