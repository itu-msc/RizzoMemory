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
  let e = let_ "x" (app (var "console_out_signal") [var "my_sig"]) (int 0) in
  let transformed = eliminate_dead_let e in
  Alcotest.(check expr_testable) "keeps dead let for effectful output call" e transformed

let test_removes_unused_top_level_let () =
  let program =
    [
      toplet "helper" (fun_ ["x"] (var "x"));
      toplet "unused" (fun_ ["x"] (binary Add (var "x") (int 1)));
      toplet "entry" (fun_ ["_"] (app (var "helper") [int 1]));
    ]
  in
  let transformed = eliminate_unused_top_levels program in
  let expected =
    [
      toplet "helper" (fun_ ["x"] (var "x"));
      toplet "entry" (fun_ ["_"] (app (var "helper") [int 1]));
    ]
  in
  Alcotest.(check program_testable) "removes unused top-level let" expected transformed

let test_keeps_transitive_top_level_dependencies () =
  let program =
    [
      toplet "a" (fun_ ["x"] (app (var "b") [var "x"]));
      toplet "b" (fun_ ["x"] (app (var "c") [var "x"]));
      toplet "c" (fun_ ["x"] (var "x"));
      toplet "unused" (fun_ ["x"] (var "x"));
      toplet "entry" (fun_ ["_"] (app (var "a") [int 1]));
    ]
  in
  let transformed = eliminate_unused_top_levels program in
  let expected =
    [
      toplet "a" (fun_ ["x"] (app (var "b") [var "x"]));
      toplet "b" (fun_ ["x"] (app (var "c") [var "x"]));
      toplet "c" (fun_ ["x"] (var "x"));
      toplet "entry" (fun_ ["_"] (app (var "a") [int 1]));
    ]
  in
  Alcotest.(check program_testable) "keeps transitive top-level dependencies" expected transformed

let test_keeps_effectful_unused_top_level_global () =
  let program =
    [
      toplet "unused_output" (app (var "console_out_signal") [var "my_sig"]);
      toplet "unused_pure" (int 1);
      toplet "entry" (fun_ ["_"] (int 0));
    ]
  in
  let transformed = eliminate_unused_top_levels program in
  let expected =
    [
      toplet "unused_output" (app (var "console_out_signal") [var "my_sig"]);
      toplet "entry" (fun_ ["_"] (int 0));
    ]
  in
  Alcotest.(check program_testable) "keeps effectful unused top-level global" expected transformed

let test_keeps_program_without_entry () =
  let program =
    [
      toplet "helper" (fun_ ["x"] (var "x"));
      toplet "unused" (int 1);
    ]
  in
  let transformed = eliminate_unused_top_levels program in
  Alcotest.(check program_testable) "keeps program without entry" program transformed

let tests_dead_let = [
  "removes nested dead lets", `Quick, test_removes_unused_nested_let;
  "keeps used let", `Quick, test_keeps_used_let;
  "respects case branch scope", `Quick, test_case_branch_scope_is_respected;
  "keeps dead let for effectful output", `Quick, test_keeps_dead_let_for_effectful_output_call;
  "removes unused top-level let", `Quick, test_removes_unused_top_level_let;
  "keeps transitive top-level dependencies", `Quick, test_keeps_transitive_top_level_dependencies;
  "keeps effectful unused top-level global", `Quick, test_keeps_effectful_unused_top_level_global;
  "keeps program without entry", `Quick, test_keeps_program_without_entry;
]
