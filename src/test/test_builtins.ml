open Rizzoc
open Rizzoc.Ast

let parse_and_typecheck input =
  let parsed = Parser.parse_string input in
  let typed, errors = typecheck parsed in
  Alcotest.(check int) "type errors" 0 (List.length errors);
  typed

let test_console_is_string_channel () =
  let typed = parse_and_typecheck "let use_console = console\n" in
  match typed with
  | [TopLet (_, EVar ("console", Ann_typed (_, console_t)), _)] ->
      Alcotest.(check bool) "console is Chan String" true (Ast.eq_typ console_t (TChan TString))
  | _ -> Alcotest.fail "unexpected typed AST shape for console builtin"

let test_parse_int_has_expected_type () =
  let typed = parse_and_typecheck "let maybe_num = parse_int(\"42\")\n" in
  match typed with
  | [TopLet (_, EApp (EVar ("parse_int", Ann_typed (_, builtin_t)), [_], Ann_typed (_, result_t)), _)] ->
      Alcotest.(check bool) "parse_int builtin type"
        true
        (Ast.eq_typ builtin_t (TFun (Cons1 (TString, []), TOption TInt)));
      Alcotest.(check bool) "parse_int return type"
        true
        (Ast.eq_typ result_t (TOption TInt))
  | _ -> Alcotest.fail "unexpected typed AST shape for parse_int builtin"

let test_clock_has_expected_type () =
  let typed = parse_and_typecheck "let ticks = clock(100)\n" in
  match typed with
  | [TopLet (_, EApp (EVar ("clock", Ann_typed (_, builtin_t)), [_], Ann_typed (_, result_t)), _)] ->
      Alcotest.(check bool) "clock builtin type"
        true
        (Ast.eq_typ builtin_t (TFun (Cons1 (TInt, []), TSignal TInt)));
      Alcotest.(check bool) "clock return type"
        true
        (Ast.eq_typ result_t (TSignal TInt))
  | _ -> Alcotest.fail "unexpected typed AST shape for clock builtin"

let test_new_builtins_have_expected_types () =
  let typed =
    parse_and_typecheck
      ("let mod_result = mod 7 3\n"
      ^ "let abs_result = abs(4 - 9)\n"
      ^ "let min_result = min 4 9\n"
      ^ "let max_result = max 4 9\n"
      ^ "let clamp_result = clamp 12 0 10\n"
      ^ "let contains_result = string_contains \"functional reactive programming\" \"reactive\"\n"
      ^ "let starts_result = string_starts_with \"hello\" \"he\"\n"
      ^ "let ends_result = string_ends_with \"hello\" \"lo\"\n")
  in
  match typed with
  | [ TopLet (_, EApp (EVar ("mod", Ann_typed (_, mod_t)), [_; _], Ann_typed (_, mod_result_t)), _);
      TopLet (_, EApp (EVar ("abs", Ann_typed (_, abs_t)), [_], Ann_typed (_, abs_result_t)), _);
      TopLet (_, EApp (EVar ("min", Ann_typed (_, min_t)), [_; _], Ann_typed (_, min_result_t)), _);
      TopLet (_, EApp (EVar ("max", Ann_typed (_, max_t)), [_; _], Ann_typed (_, max_result_t)), _);
      TopLet (_, EApp (EVar ("clamp", Ann_typed (_, clamp_t)), [_; _; _], Ann_typed (_, clamp_result_t)), _);
      TopLet (_, EApp (EVar ("string_contains", Ann_typed (_, contains_t)), [_; _], Ann_typed (_, contains_result_t)), _);
      TopLet (_, EApp (EVar ("string_starts_with", Ann_typed (_, starts_t)), [_; _], Ann_typed (_, starts_result_t)), _);
      TopLet (_, EApp (EVar ("string_ends_with", Ann_typed (_, ends_t)), [_; _], Ann_typed (_, ends_result_t)), _) ] ->
      Alcotest.(check bool) "mod builtin type" true (Ast.eq_typ mod_t (TFun (Cons1 (TInt, [TInt]), TInt)));
      Alcotest.(check bool) "mod result type" true (Ast.eq_typ mod_result_t TInt);
      Alcotest.(check bool) "abs builtin type" true (Ast.eq_typ abs_t (TFun (Cons1 (TInt, []), TInt)));
      Alcotest.(check bool) "abs result type" true (Ast.eq_typ abs_result_t TInt);
      Alcotest.(check bool) "min builtin type" true (Ast.eq_typ min_t (TFun (Cons1 (TInt, [TInt]), TInt)));
      Alcotest.(check bool) "min result type" true (Ast.eq_typ min_result_t TInt);
      Alcotest.(check bool) "max builtin type" true (Ast.eq_typ max_t (TFun (Cons1 (TInt, [TInt]), TInt)));
      Alcotest.(check bool) "max result type" true (Ast.eq_typ max_result_t TInt);
      Alcotest.(check bool) "clamp builtin type" true (Ast.eq_typ clamp_t (TFun (Cons1 (TInt, [TInt; TInt]), TInt)));
      Alcotest.(check bool) "clamp result type" true (Ast.eq_typ clamp_result_t TInt);
      Alcotest.(check bool) "string_contains builtin type" true (Ast.eq_typ contains_t (TFun (Cons1 (TString, [TString]), TBool)));
      Alcotest.(check bool) "string_contains result type" true (Ast.eq_typ contains_result_t TBool);
      Alcotest.(check bool) "string_starts_with builtin type" true (Ast.eq_typ starts_t (TFun (Cons1 (TString, [TString]), TBool)));
      Alcotest.(check bool) "string_starts_with result type" true (Ast.eq_typ starts_result_t TBool);
      Alcotest.(check bool) "string_ends_with builtin type" true (Ast.eq_typ ends_t (TFun (Cons1 (TString, [TString]), TBool)));
      Alcotest.(check bool) "string_ends_with result type" true (Ast.eq_typ ends_result_t TBool)
  | _ -> Alcotest.fail "unexpected typed AST shape for new builtins"

let builtin_tests = [
  "console is a string channel", `Quick, test_console_is_string_channel;
  "parse_int returns option int", `Quick, test_parse_int_has_expected_type;
  "clock returns signal int", `Quick, test_clock_has_expected_type;
  "new builtins have expected types", `Quick, test_new_builtins_have_expected_types;
]
