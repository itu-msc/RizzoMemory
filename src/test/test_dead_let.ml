open Rizzoc.Ast
open Rizzoc.Transformations
open Ast_test_helpers

let test_removes_unused_nested_let () =
  let e =
    let_ "x" (int 1)
      (let_ "y" (int 2) (var "x"))
  in
  let transformed = eliminate_dead_let e in
  let expected = let_ "x" (int 1) (var "x") in
  Alcotest.(check expr_testable) "removes nested dead lets" expected transformed

let test_keeps_used_let () =
  let e = let_ "x" (int 1) (binary Add (var "x") (int 2)) in
  let transformed = eliminate_dead_let e in
  Alcotest.(check expr_testable) "keeps used let" e transformed

let test_case_branch_scope_is_respected () =
  let e =
    let_ "x" (int 42)
      (case (var "v") [ (pvar "x", var "x"); (pwild, int 0) ])
  in
  let transformed = eliminate_dead_let e in
  let expected = case (var "v") [ (pvar "x", var "x"); (pwild, int 0) ] in
  Alcotest.(check expr_testable) "removes let when branch x is shadowed" expected transformed

let test_keeps_dead_let_for_effectful_output_call () =
  let e = let_ "x" (app (var "output_int_signal") [var "my_sig"]) (int 0) in
  let transformed = eliminate_dead_let e in
  Alcotest.(check expr_testable) "keeps dead let for effectful output call" e transformed

let tests_dead_let = [
  "removes nested dead lets", `Quick, test_removes_unused_nested_let;
  "keeps used let", `Quick, test_keeps_used_let;
  "respects case branch scope", `Quick, test_case_branch_scope_is_respected;
  "keeps dead let for effectful output", `Quick, test_keeps_dead_let_for_effectful_output_call;
]
