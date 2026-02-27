open Rizzoc
open Rizzoc.Ast
open Rizzoc.Transformations
open Ast_test_helpers

let test_eliminate_nested_lambda_expr () =
  let e = fun_ ["x"] (fun_ ["y"] (binary Add (var "x") (var "y"))) in
  let actual = eliminate_consecutive_lambdas e in
  let expected = fun_ ["x"; "y"] (binary Add (var "x") (var "y")) in
  Alcotest.check expr_testable "nested lambdas are merged" expected actual

let test_eliminate_nested_lambda_program () =
  let p = [toplet "f" (fun_ ["x"] (fun_ ["y"] (var "x")))] in
  let actual = eliminate_consecutive_lambdas_program p in
  let expected = [toplet "f" (fun_ ["x"; "y"] (var "x"))] in
  Alcotest.check program_testable "program-level nested lambdas are merged" expected actual

let test_eliminate_keeps_shadowing_case () =
  let e = fun_ ["x"] (fun_ ["x"] (var "x")) in
  let actual = eliminate_consecutive_lambdas e in
  let expected = fun_ ["x"] (fun_ ["x"] (var "x")) in
  Alcotest.check expr_testable "shadowing nested lambda is preserved" expected actual

let test_apply_transforms_includes_lambda_elimination () =
  let p = [toplet "f" (fun_ ["x"] (fun_ ["y"] (var "x")))] in
  let actual = apply_transforms p in
  let expected = [toplet "f" (fun_ ["x"; "y"] (var "x"))] in
  Alcotest.check program_testable "pipeline applies lambda elimination" expected actual

let lambda_tests = [
  "expr nested lambdas", `Quick, test_eliminate_nested_lambda_expr;
  "program nested lambdas", `Quick, test_eliminate_nested_lambda_program;
  "shadowing preserved", `Quick, test_eliminate_keeps_shadowing_case;
  "pipeline includes elimination", `Quick, test_apply_transforms_includes_lambda_elimination;
]
