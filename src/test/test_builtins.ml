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

let builtin_tests = [
  "console is a string channel", `Quick, test_console_is_string_channel;
  "parse_int returns option int", `Quick, test_parse_int_has_expected_type;
  "clock returns signal int", `Quick, test_clock_has_expected_type;
]
