open Rizzoc
open Rizzoc.Ast
open Rizzoc.Transformations

let program_testable = Alcotest.testable Ast.pp_program Ast.eq_program
let expr_testable = Alcotest.testable Ast.pp_expr Ast.eq_expr

let test_eliminate_nested_lambda_expr () =
  let e = EFun (["x"], EFun (["y"], EBinary (Add, EVar "x", EVar "y"))) in
  let actual = eliminate_consecutive_lambdas e in
  let expected = EFun (["x"; "y"], EBinary (Add, EVar "x", EVar "y")) in
  Alcotest.check expr_testable "nested lambdas are merged" expected actual

let test_eliminate_nested_lambda_program () =
  let p = [TLet ("f", EFun (["x"], EFun (["y"], EVar "x")))] in
  let actual = eliminate_consecutive_lambdas_program p in
  let expected = [TLet ("f", EFun (["x"; "y"], EVar "x"))] in
  Alcotest.check program_testable "program-level nested lambdas are merged" expected actual

let test_eliminate_keeps_shadowing_case () =
  let e = EFun (["x"], EFun (["x"], EVar "x")) in
  let actual = eliminate_consecutive_lambdas e in
  let expected = EFun (["x"], EFun (["x"], EVar "x")) in
  Alcotest.check expr_testable "shadowing nested lambda is preserved" expected actual

let test_apply_transforms_includes_lambda_elimination () =
  let p = [TLet ("f", EFun (["x"], EFun (["y"], EVar "x")))] in
  let actual = apply_transforms p in
  let expected = [TLet ("f", EFun (["x"; "y"], EVar "x"))] in
  Alcotest.check program_testable "pipeline applies lambda elimination" expected actual

let lambda_tests = [
  "expr nested lambdas", `Quick, test_eliminate_nested_lambda_expr;
  "program nested lambdas", `Quick, test_eliminate_nested_lambda_program;
  "shadowing preserved", `Quick, test_eliminate_keeps_shadowing_case;
  "pipeline includes elimination", `Quick, test_apply_transforms_includes_lambda_elimination;
]
